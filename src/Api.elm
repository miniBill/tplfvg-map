module Api exposing (getStops)

import Angle exposing (Angle)
import DecodeComplete
import Http
import Json.Decode exposing (Decoder)
import Json.Extra
import Types exposing (Point, Service(..), Stop)


getStops : (Result Http.Error (List Stop) -> msg) -> Cmd msg
getStops toMsg =
    Http.get
        { url =
            -- "https://tplfvg.it/services/bus-stops/all"
            "/services/bus-stops/all.json"
        , expect = Http.expectJson toMsg busStopsDecoder
        }


busStopsDecoder : Decoder (List Stop)
busStopsDecoder =
    DecodeComplete.object (\_ features -> features)
        |> DecodeComplete.required "type" (constantDecoder "FeatureCollection")
        |> DecodeComplete.required "features" (Json.Decode.list busStopDecoder)
        |> DecodeComplete.complete


constantDecoder : String -> Decoder String
constantDecoder constant =
    Json.Decode.string
        |> Json.Decode.andThen
            (\raw ->
                if raw == constant then
                    Json.Decode.succeed constant

                else
                    Json.Decode.fail
                        ("Expected "
                            ++ Json.Extra.escape constant
                            ++ ", found "
                            ++ Json.Extra.escape raw
                        )
            )


busStopDecoder : Decoder Stop
busStopDecoder =
    DecodeComplete.object
        (\_ coordinates properties ->
            { name = properties.name
            , code = properties.code
            , commune = properties.commune
            , coordinates = coordinates
            , services = properties.services
            }
        )
        |> DecodeComplete.required "type" (constantDecoder "Feature")
        |> DecodeComplete.required "geometry" pointDecoder
        |> DecodeComplete.required "properties" propertiesDecoder
        |> DecodeComplete.complete


propertiesDecoder :
    Decoder
        { name : String
        , code : String
        , commune : String
        , services : List Service
        }
propertiesDecoder =
    DecodeComplete.object
        (\name code commune services ->
            { name = name
            , code = code
            , services = services
            , commune = commune
            }
        )
        |> DecodeComplete.required "name" Json.Decode.string
        |> DecodeComplete.required "code" Json.Decode.string
        |> DecodeComplete.required "commune" Json.Decode.string
        |> DecodeComplete.discard "address"
        |> DecodeComplete.discard "marker"
        |> DecodeComplete.discard "accessibility"
        |> DecodeComplete.required "services" (Json.Decode.list serviceDecoder)
        |> DecodeComplete.complete


serviceDecoder : Decoder Service
serviceDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\_ -> ExtraUrban) (constantDecoder "extraurban")
        , Json.Decode.map (\_ -> Urban) (constantDecoder "urban")
        , Json.Decode.map (\_ -> Maritime) (constantDecoder "maritime")
        ]


pointDecoder : Decoder Point
pointDecoder =
    DecodeComplete.object (\_ point -> point)
        |> DecodeComplete.required "type" (constantDecoder "Point")
        |> DecodeComplete.required "coordinates"
            (Json.Decode.list angleDecoder
                |> Json.Decode.andThen
                    (\list ->
                        case list of
                            [ latitude, longitude ] ->
                                { latitude = latitude
                                , longitude = longitude
                                }
                                    |> Json.Decode.succeed

                            _ ->
                                Json.Decode.fail "Expected two angles"
                    )
            )
        |> DecodeComplete.complete


angleDecoder : Decoder Angle
angleDecoder =
    Json.Decode.float |> Json.Decode.map Angle.degrees
