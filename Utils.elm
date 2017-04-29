module Utils exposing (charAtIndex,State(..))

import String exposing (uncons,dropLeft)

type State
  = Failed String
  | Match
  | NoMatch

charAtIndex : String -> Int -> Result String Char
charAtIndex str index =
  case String.uncons <| dropLeft index str of
    Just (head_str,_) -> Ok head_str
    Nothing -> Err <| "invalid index of " ++ str ++ " (" ++ (toString index) ++ ")"
