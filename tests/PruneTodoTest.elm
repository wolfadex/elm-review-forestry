module PruneTodoTest exposing (all)

import PruneTodo exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "PruneTodo"
        [ test """should report an error when `Debug.todo ":prune"`""" <|
            \() ->
                """module A exposing (..)

a = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = Debug.todo ":unpruneable"
"""
                        ]
        , test """should report an error when `todo ":prune"` and all Debug functions exposed""" <|
            \() ->
                """module A exposing (..)

import Debug exposing (..)

a = todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Debug exposing (..)

a = todo ":unpruneable"
"""
                        ]
        , test """should report an error when `todo ":prune"` and Debug's todo function is exposed""" <|
            \() ->
                """module A exposing (..)

import Debug exposing (todo)

a = todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Debug exposing (todo)

a = todo ":unpruneable"
"""
                        ]
        , test """should prune a function declaration that returns a Unit value""" <|
            \() ->
                """module A exposing (..)

a : ()
a = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a : ()
a = ()
"""
                        ]
        , test """should prune a function declaration that returns an Int value""" <|
            \() ->
                """module A exposing (..)

a : Int
a = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a : Int
a = 0
"""
                        ]
        , test """should prune a function declaration that takes a `Maybe a` and returns the `a`""" <|
            \() ->
                """module A exposing (..)

a : Maybe Int -> Int
a b =
    Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a : Maybe Int -> Int
a b =
    case b of
        Nothing -> Debug.todo ":prune"
        Just arg -> Debug.todo ":prune"
"""
                        ]
        , test """should prune a function declaration that takes a `Result e a` and returns something""" <|
            \() ->
                """module A exposing (..)

a : Result e a -> a
a b =
    Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a : Result e a -> a
a b =
    case b of
        Ok argOk -> Debug.todo ":prune"
        Err argErr -> Debug.todo ":prune"
"""
                        ]
        , test """should prune a function declaration that takes a custom type and break it into its parts""" <|
            \() ->
                """module A exposing (..)

type Direction
    = Up
    | Down
    | Left
    | Right

a : Direction -> String
a dir = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

type Direction
    = Up
    | Down
    | Left
    | Right

a : Direction -> String
a dir = case dir of
            Up -> Debug.todo ":prune"
            Down -> Debug.todo ":prune"
            Left -> Debug.todo ":prune"
            Right -> Debug.todo ":prune"
"""
                        ]
        , test """should prune a function declaration that takes a complex custom type and break it into its parts""" <|
            \() ->
                """module A exposing (..)

type Direction
    = Up Int
    | Down String Int
    | Left
    | Right Direction Direction Direction

a : Direction -> String
a dir = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

type Direction
    = Up Int
    | Down String Int
    | Left
    | Right Direction Direction Direction

a : Direction -> String
a dir = case dir of
            Up arg0 -> Debug.todo ":prune"
            Down arg1 arg2 -> Debug.todo ":prune"
            Left -> Debug.todo ":prune"
            Right arg3 arg4 arg5 -> Debug.todo ":prune"
"""
                        ]
        , test """should prune a function declaration that takes an imported custom type and break it into its parts""" <|
            \() ->
                [ """module Other exposing (Direction(..))

type Direction
    = Up Int
    | Down String Int
    | Left
    | Right Direction Direction Direction
""", """module NeedsPruning exposing (..)

import Other exposing (Direction(..))

a : Direction -> String
a dir = Debug.todo ":prune"
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "NeedsPruning"
                          , [ Review.Test.error
                                { message = "Pruning..."
                                , details = [ "Prune code here?" ]
                                , under = "Debug.todo \":prune\""
                                }
                                |> Review.Test.whenFixed """module NeedsPruning exposing (..)

import Other exposing (Direction(..))

a : Direction -> String
a dir = case dir of
            Up arg0 -> Debug.todo ":prune"
            Down arg1 arg2 -> Debug.todo ":prune"
            Left -> Debug.todo ":prune"
            Right arg3 arg4 arg5 -> Debug.todo ":prune"
"""
                            ]
                          )
                        ]
        , test """shouldn't prune a function declaration that takes an imported custom type when the constructors aren't available""" <|
            \() ->
                [ """module Other exposing (Direction)

type Direction
    = Up Int
    | Down String Int
    | Left
    | Right Direction Direction Direction
""", """module NeedsPruning exposing (..)

import Other exposing (Direction(..))

a : Direction -> String
a dir = Debug.todo ":prune"
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "NeedsPruning"
                          , [ Review.Test.error
                                { message = "Pruning..."
                                , details = [ "Prune code here?" ]
                                , under = "Debug.todo \":prune\""
                                }
                                |> Review.Test.whenFixed """module NeedsPruning exposing (..)

import Other exposing (Direction(..))

a : Direction -> String
a dir = Debug.todo ":unpruneable"
"""
                            ]
                          )
                        ]
        , test """should prune a function declaration that returns a Decoder ( a, b )""" <|
            \() ->
                """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder ( Int, String )
a = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder ( Int, String )
a = Json.Decode.map2 Tuple.pair
        (Debug.todo ":prune")
        (Debug.todo ":prune")
"""
                        ]
        , test """should prune a function declaration that returns a Decoder ( a, b, c )""" <|
            \() ->
                """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder ( Int, String, () )
a = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder ( Int, String, () )
a = Json.Decode.map3
        (\\first second third -> ( first, second, third ))
        (Debug.todo ":prune")
        (Debug.todo ":prune")
        (Debug.todo ":prune")
"""
                        ]
        , test """should prune a function declaration that returns a Decoder { something : Something }""" <|
            \() ->
                """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder { left : Int, right : String }
a = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder { left : Int, right : String }
a = Json.Decode.map2
        (\\left right -> { left = left, right = right })
        (Debug.todo ":prune")
        (Debug.todo ":prune")
"""
                        ]
        , test """should prune a function declaration that returns a Decoder { r | something : Something }""" <|
            \() ->
                """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder { r | right : String }
a = Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

import Json.Decode exposing (Decoder)

a : Decoder { r | right : String }
a = Json.Decode.map
        (\\right -> { right = right })
        (Debug.todo ":prune")
"""
                        ]

        --         , test """should prune a function declaration that returns a Decoder""" <|
        --             \() ->
        --                 """module A exposing (..)
        -- import Json.Decode exposing (Decoder)
        -- type alias Model =
        --     { left : Int
        --     , right : String
        --     }
        -- a : Decoder Model
        -- a = Debug.todo ":prune"
        --         """
        --                     |> Review.Test.run rule
        --                     |> Review.Test.expectErrors
        --                         [ Review.Test.error
        --                             { message = "Pruning..."
        --                             , details = [ "Prune code here?" ]
        --                             , under = "Debug.todo \":prune\""
        --                             }
        --                             |> Review.Test.whenFixed """module A exposing (..)
        -- import Json.Decode exposing (Decoder)
        -- type alias Model =
        --     { left : Int
        --     , right : String
        --     }
        -- a : Decoder Model
        -- a = Json.Decode.map2
        --     (\\left right -> { left = left, right = right })
        --     (Debug.todo ":prune")
        --     (Debug.todo ":prune")
        -- """
        --                         ]
        , test """can build a basic `update` function""" <|
            \() ->
                """module A exposing (..)

type alias Model =
    {}

type Msg
    = Increment
    | Decrement

update : Msg -> Model -> Model
update msg model =
    Debug.todo ":prune"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Pruning..."
                            , details = [ "Prune code here?" ]
                            , under = "Debug.todo \":prune\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

type alias Model =
    {}

type Msg
    = Increment
    | Decrement

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment -> Debug.todo ":prune"
        Decrement -> Debug.todo ":prune"
"""
                        ]
        ]
