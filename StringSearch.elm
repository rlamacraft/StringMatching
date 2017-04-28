module StringSearch exposing (borderTable,searchString,State(..))

import Array exposing (Array(..),length,get,push,fromList)
import String exposing (uncons,dropLeft,length)

{-| Implementation of [Morris-Pratt String Searching](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm).

# Searching
@docs searchString, match, shiftPattern

# Border Table
@docs borderTable, newBorder, borderOfBorderLoop
-}

type State
  = Failed String
  | Match
  | NoMatch

{-| Recursive searching of a pattern on a text, using a precomputed border table -}
searchString : String -> String -> Result String (Array Int) -> Int -> State
searchString text pattern borderTable patternIndex =
  case borderTable of
    Ok table ->
      if String.length text == 0 then
        NoMatch
      else
        case match text pattern patternIndex of
          Match ->
            if patternIndex == String.length pattern - 1 then
              Match -- only attempts to find first occurance
            else
              searchString (dropLeft 1 text) pattern (Ok table) (patternIndex + 1)
          NoMatch ->
            searchString (dropLeft 1 text) pattern (Ok table) ((shiftPattern text pattern table patternIndex) + 1)
          Failed errorText ->
            Failed errorText
    Err errorText ->
      Failed errorText

{-| Checks if a specified character of the pattern matches the first character of the text -}
match : String -> String -> Int -> State
match text pattern patternIndex =
  let
    patternUncons = uncons (dropLeft patternIndex pattern)
    textUncons = uncons text
  in
    case patternUncons of
      Just (patternChar,_) ->
        case textUncons of
          Just (textChar,_) ->
            if patternChar == textChar then
              Match
            else
              NoMatch
          Nothing ->
            Failed "text is empty"
      Nothing ->
        Failed "indexing pattern error"

{-| Uses the border table to shift the window through which the pattern is compared against the text -}
shiftPattern : String -> String -> Array Int -> Int -> Int
shiftPattern text pattern borderTable patternIndex =
  let
    textChar = case uncons text of
      Just (val,_) -> val
      Nothing -> 'x'
    patternChar = case uncons (dropLeft patternIndex pattern) of
      Just (val,_) -> val
      Nothing -> 'x'
    patternIndexBorder = case get patternIndex borderTable of
      Just val -> val
      Nothing -> -1
  in
    if (patternIndex == String.length pattern) || (patternIndex >= 0 && textChar /= patternChar) then
      shiftPattern text pattern borderTable patternIndexBorder
    else
      patternIndex

{-| Entry-point wrapper for borderTableLoop -}
borderTable : String -> Result String (Array Int)
borderTable pattern =
  borderTableLoop (fromList []) pattern

{-| Recursively calculates the border table for a given pattern -}
borderTableLoop : Array Int -> String -> Result String (Array Int)
borderTableLoop table pattern =
  let
    nextChar = uncons (dropLeft (Array.length table - 1) pattern)
  in
    if Array.length table == 0 then
      borderTableLoop (push -1 table) pattern
    else if (String.length pattern) + 1 == (Array.length table) then
      Ok table
    else
      case nextChar of
        Just (char,tail) -> --not end of pattern
          case newBorder pattern table char of
            Ok newBorderValue -> borderTableLoop (push (newBorderValue + 1) table) pattern
            Err error -> Err error
        Nothing ->
          Ok (fromList [])

{-| Finds the next value of the border table -}
newBorder : String -> Array Int -> Char -> Result String Int
newBorder pattern table endingChar =
  borderOfBorderLoop pattern table endingChar
    <| get ((Array.length table) - 1) table

{-| Finds the next value of the border table -}
borderOfBorderLoop : String -> Array Int -> Char -> Maybe Int -> Result String Int
borderOfBorderLoop pattern table endingChar lastBorder =
  case lastBorder of
    Just lastBorderValue ->
      if lastBorderValue < 0 then
        Ok lastBorderValue --break loop if -1 has been reached as it is the minimum value which is the max shift
      else
        case uncons (dropLeft lastBorderValue pattern) of
          Just (headc,tailc) ->
            if headc == endingChar then
              Ok lastBorderValue --break loop if equal then border of border has been found
            else
              borderOfBorderLoop pattern table endingChar (get lastBorderValue table)
          Nothing ->
            Err "indexing pattern error"
    Nothing ->
      Err "indexing table error"
