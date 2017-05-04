module Utils exposing (charAtIndex, stringUnique, State(..))

import String exposing (uncons,dropLeft)
import Dict exposing (Dict(..), member, insert, keys)

type State
  = Failed String
  | Match
  | NoMatch

{- | Gets a specific character at a given index of a string

  charAtIndex "" 0 == Err "invalid index of (0)"
  stringUnique "abbcab" 2 == Just 'b'
-}
charAtIndex : String -> Int -> Result String Char
charAtIndex str index =
  case String.uncons <| dropLeft index str of
    Just (head_str,_) -> Ok head_str
    Nothing -> Err <| "invalid index of \"" ++ str ++ "\" (" ++ (toString index) ++ ")"

{- | Drops all occurances of each char except for the first

  stringUnique "" == ""
  stringUnique "abbcab" == "abc"
-}
stringUnique : String -> List Char
stringUnique str =
  let
    stringUnique_loop str chars_dict =
      case uncons str of
        Just (str_head, str_tail) ->
          if member str_head chars_dict then
            stringUnique_loop str_tail chars_dict
          else
            stringUnique_loop str_tail
              <| insert str_head True chars_dict
        Nothing ->
          chars_dict
  in
    keys
      <| stringUnique_loop str Dict.empty
