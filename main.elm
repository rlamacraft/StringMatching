import Html exposing (Html, input, div, label, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (id, for, style)
import Html.App as App
import Css exposing (..)

import Array exposing(Array(..),fromList,toList)
import String exposing (length,concat,uncons)
import Result exposing(withDefault)
import List exposing(head, tail, length)

import Utils exposing (stringUnique, State(..))
import StringSearch exposing (borderTable,searchString,kmpTable)
import BoyerMoore exposing (BadCharacterTable,GoodSuffixTable,initBadCharacterTable,getBadCharacterShift,initGoodSuffixTable,getGoodSuffixShift)

main : Program Never
main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

type alias Model = {
  text : String,
  pattern : String,
  borderTable : Result String (Array Int),
  kmpTable : Result String (Array Int),
  goodSuffixTable : GoodSuffixTable,
  badCharacterTable : BadCharacterTable,
  state : State
}

model : Model
model = {
  text = "",
  pattern = "",
  borderTable = Ok (fromList []),
  kmpTable = Ok (fromList []),
  goodSuffixTable = initGoodSuffixTable "",
  badCharacterTable = initBadCharacterTable "",
  state = Failed "No data"}


-- UPDATE

type Msg
  = TextInput String
  | PatternInput String


update : Msg -> Model -> Model
update msg model =
  case msg of
    TextInput newText ->
      { model | text = newText,
                state = searchString newText model.pattern model.borderTable 0 }
    PatternInput newPattern ->
      { model | pattern = newPattern,
                borderTable = borderTable newPattern,
                kmpTable = kmpTable newPattern,
                goodSuffixTable = initGoodSuffixTable newPattern,
                badCharacterTable = initBadCharacterTable newPattern,
                state = searchString model.text newPattern model.borderTable 0 }

-- VIEW
styles : List Mixin -> Html.Attribute a
styles =
    Css.asPairs >> Html.Attributes.style

pageBackground : State -> Color
pageBackground state =
  case state of
    Failed _ ->
      (rgb 250 250 250)
    Match ->
      (rgb 76 175 80)
    NoMatch ->
      (rgb 255 87 34)

printGoodSuffixTable : String -> GoodSuffixTable -> String
printGoodSuffixTable pattern table =
  let
    format index val =
      if index + 1 == String.length pattern then
        toString val
      else
        (toString val ++ ",")
    loop pattern table index htmlText =
      if index == String.length pattern then
        htmlText
      else
        loop pattern table (index + 1)
          <| (++) htmlText
          <| format index
          <| withDefault 0
          <| getGoodSuffixShift table index
  in
    "[" ++ (loop pattern table 0 "") ++ "]"

printBadCharacterTable : String -> BadCharacterTable -> String
printBadCharacterTable text table =
  let
    alphabet = stringUnique text
    format rest_length key val =
      if rest_length == 0 then
        toString key ++ ":" ++ toString val
      else
        toString key ++ ":" ++ toString val ++ ","
    loop alphabet table htmlText =
      case head alphabet of
        Just character ->
          case tail alphabet of
            Just rest ->
              loop rest table
                <| (++) htmlText
                <| format (List.length rest) character
                <| withDefault 0
                <| getBadCharacterShift table character
            Nothing ->
              htmlText
        Nothing -> ""
  in
    "{" ++ (loop alphabet table) "" ++ "}"

view : Model -> Html Msg
view model =
  div [ styles [ position absolute, left (px 0), right (px 0), top (px 0), bottom (px 0), backgroundColor (pageBackground model.state) ] ]
    [ div [ styles [ position absolute, left (pct 50), top (pct 50), marginLeft (px -135), width (px 230), marginTop (px -125), height (px 250), backgroundColor (hex "FDFDFD"), padding (px 20), borderRadius (px 3), boxShadow5 (px 0) (px 4) (px 5) (px 0) (rgba 0 0 0 0.14)] ]
      [ div [ styles [ margin (px 10) ] ] [
        label [for "text"] [Html.text "Text: "],
        input [onInput TextInput, id "text"] [] ]
      , div [ styles [ margin (px 10) ] ] [
        label [for "pattern"] [Html.text "Pattern: "],
        input [onInput PatternInput, id "pattern"] [] ]
      , div [ styles [ margin (px 10) ] ] [
        case model.borderTable of
          Ok table ->
            Html.text ("Border Table: " ++ toString (toList table))
          Err error ->
            Html.text ("Border Table: error - " ++ error)
      ]
      , div [ styles [ margin (px 10) ] ] [
        case model.kmpTable of
          Ok table ->
            Html.text ("KMP Table: " ++ toString (toList table))
          Err error ->
            Html.text ("KMP Table: error - " ++ error)
        ]
      , div [styles [ margin (px 10) ] ] [
        Html.text ("Good Suffix Table: " ++ (printGoodSuffixTable model.pattern model.goodSuffixTable))
        ]
      , div [styles [ margin (px 10) ] ] [
        Html.text ("Bad Character Table: " ++ (printBadCharacterTable model.text model.badCharacterTable))
        ]
      ]
    ]
