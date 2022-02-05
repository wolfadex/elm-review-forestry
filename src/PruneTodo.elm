module PruneTodo exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Type as Type exposing (Type)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when it encounters `Debug.todo ":prune"` or `todo ":prune"` as a placeholder

    config =
        [ PruneTodo.rule
        ]


## Failure Examples

When the code cannot be further pruned, e.g.

    a : z
    a =
        Debug.todo ":prune"

`prune` will be replaced with `unpruneable`

    a : z
    a =
        Debug.todo ":unpruneable"

This is to indicate that it has reached a dead end and further information is need to continue pruning.


## Success Examples


### Unit value

    a : ()
    a =
        Debug.todo ":prune"

will be replaced with

    a : ()
    a =
        ()


### Int

    a : Int
    a =
        Debug.todo ":prune"

will be replaced with

    a : Int
    a =
        0


### Maybe a

    a : Maybe a -> a
    a someVal =
        Debug.todo ":prune"

will be replaced with

    a : Maybe a -> a
    a someVal =
        case someVal of
            Nothing ->
                Debug.todo ":prune"

            Just arg ->
                Debug.todo ":prune"


### Result e a

    a : Result e a -> a
    a someVal =
        Debug.todo ":prune"

will be replaced with

    a : Result e a -> a
    a someVal =
        case someVal of
            Ok argOk ->
                Debug.todo ":prune"

            Err argErr ->
                Debug.todo ":prune"


## When (not) to enable this rule

This rule is useful for trying to do inline code generation.
This rule is not useful when you want to write code manually.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template wolfadex/elm-review-forestry/example --rules PruneTodo
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "PruneTodo" initialContext
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor addDeclarationsToContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor todoExpressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { debugImported : DebugImported
    , localContext : LocalContext
    , localDeclarations : List (Node Declaration)

    -- , externalDeclarations : List (Node Declaration)
    , externalDeclarations : List String
    , lookupTable : ModuleNameLookupTable
    }


type LocalContext
    = NoLocalContext
    | FuncDec Function



-- | Expr Function Expression


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { debugImported = DebugTodoWasNotImported
            , localContext = NoLocalContext
            , localDeclarations = []
            , externalDeclarations = []
            , lookupTable = lookupTable
            }
        )
        |> Rule.withModuleNameLookupTable


addDeclarationsToContext : List (Node Declaration) -> Context -> ( List (Error {}), Context )
addDeclarationsToContext declarations context =
    ( [], { context | localDeclarations = declarations } )


type DebugImported
    = DebugTodoWasNotImported
    | DebugTodoWasImported


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor node context =
    let
        newContext : Context
        newContext =
            case ( Node.value node |> .moduleName |> Node.value, (Node.value node).exposingList |> Maybe.map Node.value ) of
                ( [ "Debug" ], Just (Exposing.All _) ) ->
                    { context | debugImported = DebugTodoWasImported }

                ( [ "Debug" ], Just (Exposing.Explicit exposedFunctions) ) ->
                    let
                        isTodoFunction : Node Exposing.TopLevelExpose -> Bool
                        isTodoFunction exposeNode =
                            case Node.value exposeNode of
                                Exposing.FunctionExpose "todo" ->
                                    True

                                _ ->
                                    False
                    in
                    if List.any isTodoFunction exposedFunctions then
                        { context | debugImported = DebugTodoWasImported }

                    else
                        { context | debugImported = DebugTodoWasNotImported }

                _ ->
                    { context | debugImported = DebugTodoWasNotImported }

        newExternalDeclarations : List String
        newExternalDeclarations =
            case (Node.value node).exposingList of
                Nothing ->
                    []

                Just exposingNode ->
                    case Node.value exposingNode of
                        Exposing.All _ ->
                            []

                        Exposing.Explicit exposed ->
                            List.filterMap
                                (\exposedNode ->
                                    case Node.value exposedNode of
                                        Exposing.TypeExpose { name } ->
                                            Just name

                                        _ ->
                                            Nothing
                                )
                                exposed
    in
    ( []
    , { newContext | externalDeclarations = newExternalDeclarations ++ context.externalDeclarations }
    )


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration func ->
            ( [], { context | localContext = FuncDec func } )

        _ ->
            ( [], context )


todoExpressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
todoExpressionVisitor node context =
    case ( context.debugImported, context.localContext, Node.value node ) of
        ( DebugTodoWasImported, FuncDec func, Expression.Application [ maybeTodoNode, maybePruneNode ] ) ->
            case ( Node.value maybeTodoNode, Node.value maybePruneNode ) of
                ( Expression.FunctionOrValue [] "todo", Expression.Literal ":prune" ) ->
                    ( [ guessFunctionPruning True node context func
                      ]
                    , context
                    )

                _ ->
                    ( []
                    , context
                    )

        ( _, FuncDec func, Expression.Application [ maybeTodoNode, maybePruneNode ] ) ->
            case ( Node.value maybeTodoNode, Node.value maybePruneNode ) of
                ( Expression.FunctionOrValue [ "Debug" ] "todo", Expression.Literal ":prune" ) ->
                    ( [ guessFunctionPruning False node context func
                      ]
                    , context
                    )

                _ ->
                    ( []
                    , context
                    )

        _ ->
            ( []
            , context
            )


unpruneable : Bool -> String
unpruneable todoFuncExposed =
    if todoFuncExposed then
        "todo \":unpruneable\""

    else
        "Debug.todo \":unpruneable\""


guessFunctionPruning : Bool -> Node Expression -> Context -> Function -> Error {}
guessFunctionPruning todoFuncExposed node context func =
    Rule.errorWithFix
        { message = "Pruning..."
        , details = [ "Prune code here?" ]
        }
        (Node.range node)
        [ Fix.replaceRangeBy
            (Node.range node)
            (case func.signature of
                Nothing ->
                    unpruneable todoFuncExposed

                Just signatureNode ->
                    case Node.value signatureNode of
                        { typeAnnotation } ->
                            case Node.value typeAnnotation of
                                TypeAnnotation.Unit ->
                                    "()"

                                TypeAnnotation.Typed typeNode _ ->
                                    guessTypedPruning context todoFuncExposed typeNode

                                TypeAnnotation.FunctionTypeAnnotation inTypeAnnotation _ ->
                                    guessFunctionTypePrunig context todoFuncExposed func inTypeAnnotation

                                _ ->
                                    unpruneable todoFuncExposed
            )
        ]


guessFunctionTypePrunig : Context -> Bool -> Function -> Node TypeAnnotation -> String
guessFunctionTypePrunig context todoFuncExposed func inTypeAnnotation =
    case Node.value inTypeAnnotation of
        TypeAnnotation.Typed typeNode _ ->
            let
                funcDec : FunctionImplementation
                funcDec =
                    Node.value func.declaration
            in
            case ( Node.value typeNode, funcDec.arguments ) of
                ( ( _, "Maybe" ), [ maybeArgPattern ] ) ->
                    case ( ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode, Node.value maybeArgPattern ) of
                        ( Just [ "Maybe" ], Pattern.VarPattern maybeArg ) ->
                            "case " ++ maybeArg ++ """ of
    Nothing -> Debug.todo ":prune"
    Just arg -> Debug.todo ":prune\""""

                        _ ->
                            unpruneable todoFuncExposed

                ( ( _, "Result" ), [ resultArgPattern ] ) ->
                    case ( ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode, Node.value resultArgPattern ) of
                        ( Just [ "Result" ], Pattern.VarPattern reslutArg ) ->
                            "case " ++ reslutArg ++ """ of
    Ok argOk -> Debug.todo ":prune"
    Err argErr -> Debug.todo ":prune\""""

                        _ ->
                            unpruneable todoFuncExposed

                ( ( moduleName, typeName ), argToMatch :: _ ) ->
                    let
                        typeDeclaration : Maybe Type
                        typeDeclaration =
                            case ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode of
                                Just [] ->
                                    findDec context typeName

                                Nothing ->
                                    findDec context typeName

                                Just modName ->
                                    Debug.todo ""
                    in
                    case ( typeDeclaration, Node.value argToMatch ) of
                        ( Just declaration, Pattern.VarPattern arg ) ->
                            "case "
                                ++ arg
                                ++ " of"
                                ++ (List.foldl
                                        (\constructorNode ( result, index ) ->
                                            let
                                                constr : Type.ValueConstructor
                                                constr =
                                                    Node.value constructorNode

                                                ( constructorArgsStr, nextIndex ) =
                                                    List.foldl
                                                        (\_ ( consArgs, i ) ->
                                                            ( (" arg" ++ String.fromInt i) :: consArgs
                                                            , i + 1
                                                            )
                                                        )
                                                        ( [], index )
                                                        constr.arguments
                                            in
                                            ( result
                                                ++ "\n    "
                                                ++ Node.value constr.name
                                                ++ String.concat (List.reverse constructorArgsStr)
                                                ++ " -> Debug.todo \":prune\""
                                            , nextIndex
                                            )
                                        )
                                        ( "", 0 )
                                        declaration.constructors
                                        |> Tuple.first
                                   )

                        _ ->
                            unpruneable todoFuncExposed

                _ ->
                    unpruneable todoFuncExposed

        _ ->
            unpruneable todoFuncExposed


guessTypedPruning : Context -> Bool -> Node ( ModuleName, String ) -> String
guessTypedPruning context todoFuncExposed typeNode =
    case Node.value typeNode of
        ( _, "Int" ) ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode of
                Just [ "Basics" ] ->
                    "0"

                _ ->
                    unpruneable todoFuncExposed

        _ ->
            unpruneable todoFuncExposed


findDec : Context -> String -> Maybe Type
findDec context typeName =
    listFindBy
        (\declarationNode ->
            case Node.value declarationNode of
                Declaration.CustomTypeDeclaration ({ name } as customType) ->
                    if Node.value name == typeName then
                        Just customType

                    else
                        Nothing

                _ ->
                    Nothing
        )
        context.localDeclarations


listFindBy : (a -> Maybe b) -> List a -> Maybe b
listFindBy pred list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case pred first of
                Just a ->
                    Just a

                Nothing ->
                    listFindBy pred rest
