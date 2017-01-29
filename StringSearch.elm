module StringSearch exposing (borderTable,searchString,State(..))

import Array exposing (Array(..),length,get,push,fromList)
import String exposing (uncons,dropLeft,length)

type State
  = Failed String
  | Match
  | NoMatch

-- FIND SUBSTRING USING BORDER TABLE

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
              Match
            else
              searchString (dropLeft 1 text) pattern (Ok table) (patternIndex + 1)
          NoMatch ->
            searchString (dropLeft 1 text) pattern (Ok table) ((shiftPattern text pattern table patternIndex) + 1)
          Failed errorText ->
            Failed errorText
    Err errorText ->
      Failed errorText

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

-- CALCULATE THE BORDER TABLE

borderTable : Array Int -> String -> Result String (Array Int)
borderTable table pattern =
  let
    endingChar = uncons (dropLeft (Array.length table - 1) pattern)
  in
    if Array.length table == 0 then
      borderTable (push -1 table) pattern
    else if (String.length pattern) + 1 == (Array.length table) then
      Ok table
    else
      case endingChar of
        Just (char,tail) ->
          case newBorder pattern table char of
            Ok newBorderValue -> borderTable (push newBorderValue table) pattern
            Err error -> Err error
        Nothing ->
          Ok (fromList [])

newBorder : String -> Array Int -> Char -> Result String Int
newBorder pattern table endingChar =
  let
    lastBorder = get ((Array.length table) - 1) table
  in
    case lastBorder of
      Just num ->
        case borderOfBorderLoop pattern table endingChar num of
          Ok value -> Ok (value + 1)
          Err error -> Err error
      Nothing ->
        Err "table indexing error"


borderOfBorderLoop : String -> Array Int -> Char -> Int -> Result String Int
borderOfBorderLoop pattern table endingChar tmp =
  let
    char = uncons (dropLeft tmp pattern)
  in
    if tmp < 0 then
      Ok tmp
    else
      case char of
        Just (headc,tailc) ->
          if headc == endingChar then
            Ok tmp
          else
            case (get tmp table) of
              Just newTmp ->
                borderOfBorderLoop pattern table endingChar newTmp
              Nothing ->
                Err "tmp indexing table error"
        Nothing ->
          Err "tmp indexing pattern error"
