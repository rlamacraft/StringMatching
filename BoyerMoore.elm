module BoyerMoore exposing (BadCharacterTable,GoodSuffixTable,initBadCharacterTable,getBadCharacterShift,initGoodSuffixTable,getGoodSuffixShift,suffixTable,search)

{-| Implementation of [Boyer-Moore String Searching](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm).

# Bad Character Shift
@docs BadCharacterTable, initBadCharacterTable, getBadCharacterShift

# Good Suffix Shift
@docs GoodSuffixTable, initGoodSuffixTable, getGoodSuffixShift

# Search
@docs search
-}

import Array exposing (Array(..),fromList,get,toList)
import List exposing(reverse)
import String exposing (length,reverse,uncons,isEmpty)
import Dict exposing (Dict(..),get,empty,insert)
import Maybe exposing (withDefault)

import Utils exposing (State(..),charAtIndex)

{-| [Bad Character table](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm#The_Bad_Character_Rule) -}
type BadCharacterTable =
  BadCharacterTable Int (Dict Char Int)

{-| Generate a bad character table from a given pattern -}
initBadCharacterTable : String -> BadCharacterTable
initBadCharacterTable pattern =
  initBadCharacterTable_loop pattern
    <| BadCharacterTable (length pattern) Dict.empty

initBadCharacterTable_loop : String -> BadCharacterTable -> BadCharacterTable
initBadCharacterTable_loop pattern (BadCharacterTable patternLength table) =
  if String.length pattern == 1 then
    BadCharacterTable patternLength table
  else
    case uncons pattern of
      Just (patternHead,patternTail) ->
        initBadCharacterTable_loop patternTail
          <| BadCharacterTable patternLength
          <| insert patternHead (length pattern - 1) table
      Nothing ->
        BadCharacterTable 0 empty

{-| Get a value from a bad character table
    Uses pattern length as default value, if requested character does not occur in the pattern
-}
getBadCharacterShift : BadCharacterTable -> Char -> Result String Int
getBadCharacterShift (BadCharacterTable patternLength table) requestChar =
  Ok
    <| withDefault patternLength
    <| Dict.get requestChar table

{-| [Good Suffix table](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm#The_Good_Suffix_Rule) -}
type GoodSuffixTable =
  GoodSuffixTable (Array Int)

{-| Generate a good suffix table from a given pattern -}
initGoodSuffixTable : String -> GoodSuffixTable
initGoodSuffixTable pattern =
  let
    reverseTable (GoodSuffixTable table) =
      table
        |> Array.toList
        |> List.reverse
        |> Array.fromList
        |> GoodSuffixTable
  in
     GoodSuffixTable (fromList [])
        |> initGoodSuffixTable_firstLoop pattern (suffixTable pattern) (length pattern - 2)
        |> reverseTable
        |> initGoodSuffixTable_secondLoop (suffixTable pattern) (length pattern) 0


initGoodSuffixTable_firstLoop : String -> (Array Int) -> Int -> GoodSuffixTable -> GoodSuffixTable
initGoodSuffixTable_firstLoop pattern suffixTable index (GoodSuffixTable table) =
  let
    m = String.length pattern
    appendedGoodSuffixTable table additions =
      Array.append additions table
        |> GoodSuffixTable
  in
    if index == -1 then
      Array.repeat (m - Array.length table) m
        |> appendedGoodSuffixTable table
    else
      case Array.get index suffixTable of
        Just suff_i ->
          if suff_i == index + 1 then
            Array.repeat (m - suff_i - Array.length table) (m - suff_i)
              |> appendedGoodSuffixTable table
              |> initGoodSuffixTable_firstLoop pattern suffixTable (index - 1)
          else
            GoodSuffixTable table
              |> initGoodSuffixTable_firstLoop pattern suffixTable (index - 1)
        Nothing ->
          GoodSuffixTable <| Array.fromList []


initGoodSuffixTable_secondLoop : (Array Int) -> Int -> Int -> GoodSuffixTable -> GoodSuffixTable
initGoodSuffixTable_secondLoop suffixTable patternLength index (GoodSuffixTable table) =
  if index == patternLength - 1 then
    GoodSuffixTable table
  else
    case Array.get index suffixTable of
      Just suff_i ->
        Array.set (patternLength - 1 - suff_i) (patternLength - 1 - index) table
          |> GoodSuffixTable
          |> initGoodSuffixTable_secondLoop suffixTable patternLength (index + 1)
      Nothing ->
        GoodSuffixTable table

{-| Generate a suffix table of a given pattern -}
suffixTable : String -> Array Int
suffixTable pattern =
  if isEmpty pattern then
    fromList []
  else
    suffixTable_loop pattern (fromList [length pattern]) (length pattern - 2) 0 (length pattern - 1)
      |> toList
      |> List.reverse
      |> fromList

{-| Recursively generate each value of the suffix table. Table is reversed. -}
suffixTable_loop : String -> (Array Int) -> Int -> Int -> Int -> (Array Int)
suffixTable_loop pattern table i j k =
  let
    tmp = Array.get (i + (length pattern) - j - i) table
    new_k = new_k_generator pattern i (min i k)
  in
    if i < 0 then
      table
    else if i > k then
      case tmp of
        Just value ->
          if value /= i - k then
            suffixTable_loop pattern (Array.push (min value (i - k)) table) (i - 1) j k
          else
            suffixTable_loop pattern (Array.push (i - new_k) table) (i - 1) i k
        Nothing ->
          table
    else
      suffixTable_loop pattern (Array.push (i - new_k) table) (i - 1) i k

{-| Used as part of generating the suffix table for calculating parameter of recurisve call -}
new_k_generator : String -> Int -> Int -> Int
new_k_generator pattern j k =
  if k < 0 then
    k
  else
    case charAtIndex pattern k of
      Ok pattern_k ->
        case charAtIndex pattern (k + (length pattern) - j - 1) of
          Ok pattern_tmp ->
            if pattern_k == pattern_tmp then
              new_k_generator pattern j (k - 1)
            else
              k
          Err error -> k
      Err error -> k

{-| Get a value from a good suffix table
    Will return error if requested index is greater than length of pattern or less than 0
-}
getGoodSuffixShift : GoodSuffixTable -> Int -> Result String Int
getGoodSuffixShift (GoodSuffixTable table) index =
  case Array.get index table of
    Just value -> Ok value
    Nothing -> Err "invalid index arg"

{- | Search function using the GoodSuffix and BadCharacter tables -}
search : String -> String -> GoodSuffixTable -> BadCharacterTable -> State
search text pattern goodSuffix badCharacter =
  searchLoop text pattern goodSuffix badCharacter (String.length pattern - 1) 0

searchLoop : String -> String -> GoodSuffixTable -> BadCharacterTable -> Int -> Int -> State
searchLoop text pattern goodSuffix badCharacter index pos =
  if pos > String.length text - String.length pattern then
    NoMatch
  else if index == -1 then
    Match
  else
    case charAtIndex text (pos + index) of
      Ok text_char ->
        case charAtIndex pattern index of
          Ok pattern_char ->
            if text_char == pattern_char then
              searchLoop text pattern goodSuffix badCharacter (index - 1) pos
            else
              case getGoodSuffixShift goodSuffix index of
                Ok goodSuffixShift ->
                  case getBadCharacterShift badCharacter text_char of
                    Ok badCharacterShift ->
                      searchLoop text pattern goodSuffix badCharacter (String.length pattern - 1)
                        <| (+) pos
                        <| max goodSuffixShift
                        <| badCharacterShift - String.length pattern + 1 + index
                    Err error -> Failed error
                Err error -> Failed error
          Err error -> Failed error
      Err error -> Failed error
