module Types.Score exposing (..)

type alias Score
    = (Int, Int)

init : Score
init =
    (0, 0)

succeed : Score -> Score
succeed (x, y) =
    (x + 1, y + 1)

fail : Score -> Score
fail (x, y) =
    (x, y + 1)

toText : Score -> String
toText (x, y) =
    (toString x) ++ "/" ++ (toString y)
