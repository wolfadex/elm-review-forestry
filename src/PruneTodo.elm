module PruneTodo exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type as Type exposing (Type)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, TypeAnnotation)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
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


### Custom types

Given
module Other exposing (Direction(..))

    type Direction
        = Up Int
        | Down String Int
        | Left
        | Right Direction Direction Direction

and
module This exposing (a)

    directionToText : Direction -> String
    directionToText direction =
        Debug.todo ":prune"

`directionToText` will be updated to

    directionToText : Direction -> String
    directionToText direction =
        case direction of
            Up arg0 ->
                Debug.todo ":prune"

            Down arg1 arg2 ->
                Debug.todo ":prune"

            Left ->
                Debug.todo ":prune"

            Right arg3 arg4 arg5 ->
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
    Rule.newProjectRuleSchema "PruneTodo - Project" { exposedDeclarations = Dict.empty }
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule =
                Rule.initContextCreator toModuleContext
                    |> Rule.withModuleNameLookupTable
            , fromModuleToProject =
                Rule.initContextCreator toProjectContext
                    |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedDeclarations = Dict.union previousContext.exposedDeclarations newContext.exposedDeclarations
    }


moduleVisitor : Rule.ModuleRuleSchema moduleSchemaState ModuleContext -> Rule.ModuleRuleSchema { moduleSchemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor moduleSchema =
    moduleSchema
        |> Rule.withImportVisitor importVisitor
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor addDeclarationsToContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor todoExpressionVisitor


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor node moduleContext =
    ( []
    , { moduleContext
        | toExpose = Module.exposingList (Node.value node)
      }
    )


type alias ModuleContext =
    { debugImported : DebugImported
    , localContext : LocalContext
    , localDeclarations : List (Node Declaration)
    , toExpose : Exposing
    , exposedDeclarations : List (Node Declaration)
    , externalDeclarations : Dict String (List (Node Declaration))
    , lookupTable : ModuleNameLookupTable
    }


type LocalContext
    = NoLocalContext
    | FuncDec Function


type alias ProjectContext =
    { exposedDeclarations : Dict String (List (Node Declaration))
    }



-- | Expr Function Expression


toProjectContext : Rule.Metadata -> ModuleContext -> ProjectContext
toProjectContext metadata moduleContext =
    { exposedDeclarations = Dict.singleton (String.join "." (Rule.moduleNameFromMetadata metadata)) moduleContext.exposedDeclarations
    }


toModuleContext : ModuleNameLookupTable -> ProjectContext -> ModuleContext
toModuleContext lookupTable projectContext =
    { debugImported = DebugTodoWasNotImported
    , localContext = NoLocalContext
    , localDeclarations = []
    , toExpose = Exposing.All { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
    , exposedDeclarations = []
    , externalDeclarations = projectContext.exposedDeclarations
    , lookupTable = lookupTable
    }


addDeclarationsToContext : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
addDeclarationsToContext declarations context =
    ( []
    , { context
        | localDeclarations = declarations
        , exposedDeclarations =
            case context.toExpose of
                Exposing.All _ ->
                    declarations

                Exposing.Explicit exposedValues ->
                    List.filterMap
                        (\exposedNode ->
                            case Node.value exposedNode of
                                Exposing.TypeExpose { name, open } ->
                                    case open of
                                        Nothing ->
                                            Nothing

                                        Just _ ->
                                            listFindBy
                                                (\declarationNode ->
                                                    case Node.value declarationNode of
                                                        Declaration.CustomTypeDeclaration customType ->
                                                            if Node.value customType.name == name then
                                                                Just declarationNode

                                                            else
                                                                Nothing

                                                        _ ->
                                                            Nothing
                                                )
                                                declarations

                                Exposing.TypeOrAliasExpose name ->
                                    listFindBy
                                        (\declarationNode ->
                                            case Node.value declarationNode of
                                                Declaration.AliasDeclaration typeAlias ->
                                                    if Node.value typeAlias.name == name then
                                                        Just declarationNode

                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                        )
                                        declarations

                                _ ->
                                    Nothing
                        )
                        exposedValues
      }
    )


type DebugImported
    = DebugTodoWasNotImported
    | DebugTodoWasImported


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor node context =
    ( []
    , case ( Node.value node |> .moduleName |> Node.value, (Node.value node).exposingList |> Maybe.map Node.value ) of
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
    )


declarationVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration func ->
            ( [], { context | localContext = FuncDec func } )

        _ ->
            ( [], context )


todoExpressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
todoExpressionVisitor node context =
    case ( context.debugImported, context.localContext, Node.value node ) of
        ( DebugTodoWasImported, FuncDec func, Expression.Application [ maybeTodoNode, maybePruneNode ] ) ->
            case ( Node.value maybeTodoNode, Node.value maybePruneNode ) of
                ( Expression.FunctionOrValue [] "todo", Expression.Literal ":prune" ) ->
                    ( [ guessFunctionPruning (Node.range node) True node context func
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
                    ( [ guessFunctionPruning (Node.range node) False node context func
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


guessFunctionPruning : Range -> Bool -> Node Expression -> ModuleContext -> Function -> Error {}
guessFunctionPruning replaceRange todoFuncExposed node context func =
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

                                TypeAnnotation.Typed typeNode wrappedAnnotations ->
                                    guessTypedPruning replaceRange context todoFuncExposed typeNode wrappedAnnotations

                                TypeAnnotation.FunctionTypeAnnotation inTypeAnnotation outTypeAnnotation ->
                                    guessFunctionTypePrunig replaceRange context todoFuncExposed func inTypeAnnotation outTypeAnnotation

                                _ ->
                                    unpruneable todoFuncExposed
            )
        ]


getIndentation : Range -> String
getIndentation replaceRange =
    let
        indentationOffset : Int
        indentationOffset =
            remainderBy 4 (replaceRange.start.column - 1)
    in
    String.repeat
        ((if indentationOffset == 0 then
            replaceRange.start.column

          else
            replaceRange.start.column - indentationOffset
         )
            + 3
        )
        " "


guessFunctionTypePrunig : Range -> ModuleContext -> Bool -> Function -> Node TypeAnnotation -> Node TypeAnnotation -> String
guessFunctionTypePrunig replaceRange context todoFuncExposed func inTypeAnnotation outTypeAnnotation =
    case Node.value inTypeAnnotation of
        TypeAnnotation.Typed typeNode _ ->
            let
                funcDec : FunctionImplementation
                funcDec =
                    Node.value func.declaration

                indentation : String
                indentation =
                    getIndentation replaceRange
            in
            case ( Node.value typeNode, funcDec.arguments ) of
                ( ( _, "Maybe" ), [ maybeArgPattern ] ) ->
                    case ( ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode, Node.value maybeArgPattern ) of
                        ( Just [ "Maybe" ], Pattern.VarPattern maybeArg ) ->
                            "case "
                                ++ maybeArg
                                ++ " of\n"
                                ++ indentation
                                ++ "Nothing -> Debug.todo \":prune\"\n"
                                ++ indentation
                                ++ "Just arg -> Debug.todo \":prune\""

                        _ ->
                            unpruneable todoFuncExposed

                ( ( _, "Result" ), [ resultArgPattern ] ) ->
                    case ( ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode, Node.value resultArgPattern ) of
                        ( Just [ "Result" ], Pattern.VarPattern reslutArg ) ->
                            "case "
                                ++ reslutArg
                                ++ " of\n"
                                ++ indentation
                                ++ "Ok argOk -> Debug.todo \":prune\"\n"
                                ++ indentation
                                ++ "Err argErr -> Debug.todo \":prune\""

                        _ ->
                            unpruneable todoFuncExposed

                ( ( _, "Decoder" ), [] ) ->
                    case ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode of
                        Just [ "Json", "Decode" ] ->
                            case Node.value outTypeAnnotation of
                                TypeAnnotation.Unit ->
                                    "Json.Decode.succeed ()"

                                TypeAnnotation.Tupled tupleTypeAnnotations ->
                                    case tupleTypeAnnotations of
                                        [ _, _ ] ->
                                            "Json.Decode.map2 Tuple.pair\n"
                                                ++ indentation
                                                ++ "(Debug.todo \":prune\")\n"
                                                ++ indentation
                                                ++ "(Debug.todo \":prune\")"

                                        [ _, _, _ ] ->
                                            "Json.Decode.map3\n"
                                                ++ indentation
                                                ++ "(\\first second third -> ( first, second, third ))\n"
                                                ++ indentation
                                                ++ "(Debug.todo \":prune\")\n"
                                                ++ indentation
                                                ++ "(Debug.todo \":prune\")\n"
                                                ++ indentation
                                                ++ "(Debug.todo \":prune\")"

                                        _ ->
                                            unpruneable todoFuncExposed

                                TypeAnnotation.Record recordDefinition ->
                                    Debug.todo "TODO"

                                TypeAnnotation.GenericRecord extensibleRecordNameNode recordDefinitionNode ->
                                    Debug.todo "TODO"

                                _ ->
                                    unpruneable todoFuncExposed

                        _ ->
                            unpruneable todoFuncExposed

                ( ( _, typeName ), argToMatch :: _ ) ->
                    let
                        typeDeclaration : Maybe Type
                        typeDeclaration =
                            case ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode of
                                Just [] ->
                                    findDec context.localDeclarations typeName

                                Nothing ->
                                    findDec context.localDeclarations typeName

                                Just modName ->
                                    case Dict.get (String.join "." modName) context.externalDeclarations of
                                        Nothing ->
                                            Nothing

                                        Just externalDeclarations ->
                                            findDec externalDeclarations typeName
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
                                                ++ "\n"
                                                ++ indentation
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


guessTypedPruning : Range -> ModuleContext -> Bool -> Node ( ModuleName, String ) -> List (Node TypeAnnotation) -> String
guessTypedPruning replaceRange context todoFuncExposed typeNode wrappedAnnotations =
    case Node.value typeNode of
        ( _, "Int" ) ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode of
                Just [ "Basics" ] ->
                    "0"

                _ ->
                    unpruneable todoFuncExposed

        ( _, "Decoder" ) ->
            case ModuleNameLookupTable.moduleNameFor context.lookupTable typeNode of
                Just [ "Json", "Decode" ] ->
                    let
                        indentation : String
                        indentation =
                            getIndentation replaceRange
                    in
                    case List.map Node.value wrappedAnnotations of
                        [ TypeAnnotation.Unit ] ->
                            "Json.Decode.succeed ()"

                        [ TypeAnnotation.Tupled tupleTypeAnnotations ] ->
                            case tupleTypeAnnotations of
                                [ _, _ ] ->
                                    "Json.Decode.map2 Tuple.pair\n"
                                        ++ indentation
                                        ++ "(Debug.todo \":prune\")\n"
                                        ++ indentation
                                        ++ "(Debug.todo \":prune\")"

                                [ _, _, _ ] ->
                                    "Json.Decode.map3\n"
                                        ++ indentation
                                        ++ "(\\first second third -> ( first, second, third ))\n"
                                        ++ indentation
                                        ++ "(Debug.todo \":prune\")\n"
                                        ++ indentation
                                        ++ "(Debug.todo \":prune\")\n"
                                        ++ indentation
                                        ++ "(Debug.todo \":prune\")"

                                _ ->
                                    unpruneable todoFuncExposed

                        [ TypeAnnotation.Record recordDefinition ] ->
                            generateRecordDecoder indentation todoFuncExposed recordDefinition

                        [ TypeAnnotation.GenericRecord _ recordDefinitionNode ] ->
                            generateRecordDecoder indentation todoFuncExposed (Node.value recordDefinitionNode)

                        _ ->
                            unpruneable todoFuncExposed

                _ ->
                    unpruneable todoFuncExposed

        _ ->
            unpruneable todoFuncExposed


generateRecordDecoder : String -> Bool -> RecordDefinition -> String
generateRecordDecoder indentation todoFuncExposed recordDefinition =
    let
        quantityToDecode : Int
        quantityToDecode =
            List.length recordDefinition
    in
    if quantityToDecode == 1 then
        "Json.Decode.map\n"
            ++ indentation
            ++ "(\\"
            ++ String.join " " (List.map (Node.value >> Tuple.first >> Node.value) recordDefinition)
            ++ " -> { "
            ++ String.join ", "
                (List.map
                    (\recordField ->
                        let
                            ( labelNode, _ ) =
                                Node.value recordField

                            label : String
                            label =
                                Node.value labelNode
                        in
                        label ++ " = " ++ label
                    )
                    recordDefinition
                )
            ++ " })"
            ++ String.concat (List.repeat quantityToDecode ("\n" ++ indentation ++ "(Debug.todo \":prune\")"))

    else if quantityToDecode < 9 then
        "Json.Decode.map"
            ++ String.fromInt quantityToDecode
            ++ "\n"
            ++ indentation
            ++ "(\\"
            ++ String.join " " (List.map (Node.value >> Tuple.first >> Node.value) recordDefinition)
            ++ " -> { "
            ++ String.join ", "
                (List.map
                    (\recordField ->
                        let
                            ( labelNode, _ ) =
                                Node.value recordField

                            label : String
                            label =
                                Node.value labelNode
                        in
                        label ++ " = " ++ label
                    )
                    recordDefinition
                )
            ++ " })"
            ++ String.concat (List.repeat quantityToDecode ("\n" ++ indentation ++ "(Debug.todo \":prune\")"))

    else if Debug.todo "decode pipeline available" then
        Debug.todo "TODO"

    else if Debug.todo "decode extra available" then
        Debug.todo "TODO"

    else
        unpruneable todoFuncExposed


findDec : List (Node Declaration) -> String -> Maybe Type
findDec declarations typeName =
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
        declarations


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
