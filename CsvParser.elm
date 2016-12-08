module CsvParser exposing (parseCsv)

import Regex exposing (..)

parseCsv : String -> List (List String)
parseCsv csv =
    let
        trimTrailingComma =
            Regex.replace (AtMost 1) (regex ",$") (\_ -> "")
        splitOnComma =
            String.split ","
    in
        csv
            |> String.lines
            |> List.map (trimTrailingComma >> splitOnComma)
