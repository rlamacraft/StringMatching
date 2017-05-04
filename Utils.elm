module Utils exposing (charAtIndex,State(..))

import String exposing (uncons,dropLeft)

type State
  = Failed String
  | Match
  | NoMatch

{- | Gets a specific character at a given index of a string

  charAtIndex "" 0 = Err "invalid index of (0)"
  stringUnique "abbcab" 2 = Just 'b'
-}
charAtIndex : String -> Int -> Result String Char
charAtIndex str index =
  case String.uncons <| dropLeft index str of
    Just (head_str,_) -> Ok head_str
    Nothing -> Err <| "invalid index of \"" ++ str ++ "\" (" ++ (toString index) ++ ")"
