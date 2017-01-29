import Html exposing (Html, input, div, label, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (id, for, style)
import Html.App as App
import Css exposing (..)

import Array exposing(Array(..),fromList,toList)

import StringSearch exposing (borderTable,searchString,State(..))


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
  state : State
}

model : Model
model = {
  text = "",
  pattern = "",
  borderTable = Ok (fromList []),
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
                borderTable = borderTable (fromList []) newPattern,
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

view : Model -> Html Msg
view model =
  div [ styles [ position absolute, left (px 0), right (px 0), top (px 0), bottom (px 0), backgroundColor (pageBackground model.state) ] ]
    [ div [ styles [ position absolute, left (pct 50), top (pct 50), marginLeft (px -135), width (px 230), marginTop (px -75), height (px 110), backgroundColor (hex "FDFDFD"), padding (px 20), borderRadius (px 3), boxShadow5 (px 0) (px 4) (px 5) (px 0) (rgba 0 0 0 0.14)] ]
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
      ]
    ]
