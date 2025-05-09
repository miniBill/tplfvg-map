module Json.Extra exposing (escape)

import Json.Encode


escape : String -> String
escape input =
    input
        |> Json.Encode.string
        |> Json.Encode.encode 0
