port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect exposing (..)
import Array exposing (Array(..),fromList)

import StringSearch exposing(borderTable, searchString, State(..))

all : Test
all =
  describe "all tests"
    [ borderTableTests
    , searchStringTests
    ]

main : Program Value
main =
  run emit all

borderTableTests : Test
borderTableTests =
    describe "Border Table for Morris-Pratt"
        [ test "Empty String" <|
          \() ->
            let
              emptyStringBorderTable = Ok <| Array.fromList [ -1 ]
            in
              borderTable ""
                |> Expect.equal emptyStringBorderTable
        , test "SimpleStrin" <|
          \() ->
            let
              simpleStringBorderTable = Ok <| Array.fromList [ -1, 0, 0, 1, 2]
            in
              borderTable "abab"
                |> Expect.equal simpleStringBorderTable
        ]

searchStringTests : Test
searchStringTests =
  describe "String Search Tests using Morris-Pratt"
    [ test "Empty String" <|
      \() ->
        let
          emptyStringBorderTable = Ok <| Array.fromList [ -1 ]
        in
          searchString "" "" emptyStringBorderTable 0
            |> Expect.equal NoMatch
    , test "Some String" <|
      \() ->
        let
          simpleStringBorderTable = Ok <| Array.fromList [ -1, 0, 0, 1, 2]
        in
          searchString "abcabab" "abab" simpleStringBorderTable 0
            |> Expect.equal Match
    ]

port emit : ( String, Value ) -> Cmd msg
