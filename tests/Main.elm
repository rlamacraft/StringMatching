port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect exposing (..)
import Array exposing (Array(..),fromList)

import Utils exposing (State(..))
import StringSearch exposing (borderTable, searchString, kmpTable)
import BoyerMoore exposing (BadCharacterTable, initBadCharacterTable, getBadCharacterShift)

all : Test
all =
  describe "all tests"
    [ borderTableTests
    , searchStringTests
    , kmpTableTests
    , badCharacterTests
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

kmpTableTests : Test
kmpTableTests =
  describe "Pre-computed KMP Table for Knuth-Morris-Pratt string searching"
      [ test "Empty String" <|
        \() ->
          let
            emptyStringBorderTable = Ok <| Array.fromList [ -1, 0 ]
          in
            kmpTable ""
              |> Expect.equal emptyStringBorderTable
      , test "Simple String" <|
        \() ->
          let
            simpleStringBorderTable = Ok <| Array.fromList [ -1, 0, -1, 0, 2 ]
          in
            kmpTable "abab"
              |> Expect.equal simpleStringBorderTable
      , test "More Complex String" <|
        \() ->
          let
            moreComplexStringBorderTable = Ok <| Array.fromList [ -1, 0, -1, 1, -1, 0, -1, 1, -1, 0, 6 ]
          in
            kmpTable "abacabacab"
              |> Expect.equal moreComplexStringBorderTable
      ]

badCharacterTests : Test
badCharacterTests =
  describe "Generate the bad character table for Boyer-Moore"
      [ test "Empty String" <|
        \() ->
          let
            emptyBadCharacterTable = initBadCharacterTable ""
          in
            getBadCharacterShift emptyBadCharacterTable 'a'
              |> Expect.equal (Ok 0)
      , test "Simple String" <|
        \() ->
          let
            emptyBadCharacterTable = initBadCharacterTable "ababcabab"
          in
            [ getBadCharacterShift emptyBadCharacterTable 'a'
            , getBadCharacterShift emptyBadCharacterTable 'b'
            , getBadCharacterShift emptyBadCharacterTable 'c'
            , getBadCharacterShift emptyBadCharacterTable 'd'
            ]
              |> Expect.equal [ Ok 1, Ok 2, Ok 4, Ok 9]
      ]

port emit : ( String, Value ) -> Cmd msg
