import Html exposing (Html, input, div, label, text, main', h1, label, h2, span)
import Html.Events exposing (onInput)
import Html.Attributes exposing (attribute,class)
import Html.App as App

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
            Nothing -> htmlText
        Nothing -> htmlText
  in
    "{" ++ (loop alphabet table) "" ++ "}"

view : Model -> Html Msg
view model =
  let
    stateString = case model.state of
      Failed _ -> "failed"
      Match -> "match"
      NoMatch -> "nomatch"
  in
    main' []
      [ div [ class "inputs", attribute "state" stateString ]
          [ label []
            [ text "Pattern"
            , input [onInput PatternInput] []
            ]
          , label []
            [ text "Text"
            , input [onInput TextInput] []
            ]
          ]
      , div [ class "outputs" ]
          [ div [ class "output" ]
            [ h1 [] [ text "Knuth-Morris-Pratt" ]
            , div []
              [ h2 [] [ text "Border Table" ]
              , span [] [
                case model.borderTable of
                    Ok table ->
                      Html.text (toString (toList table))
                    Err error ->
                      Html.text ("error - " ++ error)
                ]
              ]
            , div []
              [ h2 [] [ text "KMP Table" ]
              , span [] [
                case model.kmpTable of
                  Ok table ->
                    Html.text (toString (toList table))
                  Err error ->
                    Html.text ("error - " ++ error)
                ]
              ]
            ]
          , div [ class "output" ]
            [ h1 [] [ text "Boyer-Moore" ]
            , div []
              [ h2 [] [ text "Good Suffix Table" ]
              , span [] [
                  text <| printGoodSuffixTable model.pattern model.goodSuffixTable
                ]
              ]
            , div []
              [ h2 [] [ text "Bad Character Table" ]
              , span [] [
                  text <| printBadCharacterTable model.text model.badCharacterTable
                ]
              ]
            ]
          ]
      ]
