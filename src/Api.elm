module Api exposing (getStops)

import DecodeComplete
import Http
import Json.Decode exposing (Decoder)
import Types exposing (Stop)


getStops : (Result Http.Error (List Stop) -> msg) -> Cmd msg
getStops toMsg =
    Http.request
        { url = "https://tplfvg.it/services/bus-stops/all"
        , expect = Http.expectJson toMsg (Json.Decode.list busStopsDecoder)
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = []
        }


busStopsDecoder : Decoder Stop
busStopsDecoder =
    DecodeComplete.object Stop
        |> DecodeComplete.complete
