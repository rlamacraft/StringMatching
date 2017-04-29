module BoyerMoore exposing (BadCharacterTable,GoodSuffixTable,initBadCharacterTable,getBadCharacterShift,initGoodSuffixTable,getGoodSuffixShift)

{-| Implementation of [Boyer-Moore String Searching](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm).

# Bad Character Shift
@docs BadCharacterTable, initBadCharacterTable, getBadCharacterShift

# Good Suffix Shift
@docs GoodSuffixTable, initGoodSuffixTable, getGoodSuffixShift
-}

import Array exposing (Array(..),fromList)
import String exposing (length)
import Dict exposing (Dict(..),get,empty)
import Maybe exposing (withDefault)

{-| [Bad Character table](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm#The_Bad_Character_Rule) -}
type BadCharacterTable =
  BadCharacterTable Int (Dict Char Int)

{-| Generate a bad character table from a given pattern -}
initBadCharacterTable : String -> BadCharacterTable
initBadCharacterTable pattern =
  BadCharacterTable (String.length pattern) (Dict.empty) --replace this Dict.empty with a generated dict

{-| Get a value from a bad character table
    Uses pattern length as default value, if requested character does not occur in the pattern
-}
getBadCharacterShift : BadCharacterTable -> Char -> Result String Int
getBadCharacterShift (BadCharacterTable patternLength table) requestChar =
  Ok
    <| Maybe.withDefault patternLength
    <| Dict.get requestChar table

{-| [Good Suffix table](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm#The_Good_Suffix_Rule) -}
type GoodSuffixTable =
  GoodSuffixTable (Array Int)

{-| Generate a good suffix table from a given pattern -}
initGoodSuffixTable : String -> GoodSuffixTable
initGoodSuffixTable pattern =
  GoodSuffixTable <| Array.fromList [] --replace this empty array with a generated array

{-| Get a value from a good suffix table
    Will return error if requested index is greater than length of pattern or less than 0
-}
getGoodSuffixShift : GoodSuffixTable -> Int -> Result String Int
getGoodSuffixShift (GoodSuffixTable table) index =
  case Array.get index table of
    Just value -> Ok value
    Nothing -> Err "invalid index arg"
