module Api exposing (getBusesForStopId, getStops)

import Angle exposing (Angle)
import DecodeComplete
import Duration exposing (Duration)
import Http
import Id exposing (Id, Stop, Vehicle)
import Json.Decode exposing (Decoder)
import Json.Encode
import Process
import Quantity
import Task
import Types exposing (BackendMsg(..), Bus, Point, Service(..), StopInfo)


getStops : Cmd BackendMsg
getStops =
    Http.get
        { url = "https://tplfvg.it/services/geojson/points/"
        , expect = Http.expectJson GotStops busStopsDecoder
        }


getBusesForStopId : Duration -> Id Stop -> Cmd BackendMsg
getBusesForStopId sleepTime stopId =
    Process.sleep (Duration.inMilliseconds sleepTime)
        |> Task.andThen
            (\_ ->
                Http.task
                    { url =
                        "https://realtime.tplfvg.it/API/v1.0/polemonitor/mrcruns?StopCode=" ++ Id.toString stopId ++ "&IsUrban=true"
                    , method = "GET"
                    , timeout = Nothing
                    , resolver = jsonResolver (busesDecoder stopId)
                    , body = Http.emptyBody
                    , headers = []
                    }
            )
        |> Task.attempt (GotBusesFromStop stopId)


jsonResolver : Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ { statusCode } _ ->
                    Err (Http.BadStatus statusCode)

                Http.GoodStatus_ _ body ->
                    case Json.Decode.decodeString decoder body of
                        Ok result ->
                            Ok result

                        Err jsonError ->
                            Err (Http.BadBody (Json.Decode.errorToString jsonError))
        )


busesDecoder : Id Stop -> Decoder (List ( Id Vehicle, Bus ))
busesDecoder stopId =
    (DecodeComplete.object
        (\departure destination latitude longitude vehicle line ->
            if
                (longitude == Quantity.zero)
                    && (latitude == Quantity.zero)
            then
                Nothing

            else
                ( vehicle
                , { name =
                        if String.isEmpty departure then
                            destination

                        else
                            departure ++ " => " ++ destination
                  , line = line
                  , coordinates =
                        { latitude = latitude
                        , longitude = longitude
                        }
                  , stop = stopId
                  }
                )
                    |> Just
        )
        |> DecodeComplete.required "Departure" Json.Decode.string
        |> DecodeComplete.required "Destination" Json.Decode.string
        |> DecodeComplete.required "Latitude" angleDecoder
        |> DecodeComplete.required "Longitude" angleDecoder
        |> DecodeComplete.required "Vehicle" (Json.Decode.map Id.fromString Json.Decode.string)
        |> DecodeComplete.required "LineCode" (Json.Decode.map Id.fromString Json.Decode.string)
        |> DecodeComplete.discard "Line"
        |> DecodeComplete.discard "Time"
        |> DecodeComplete.discard "DepartureTime"
        |> DecodeComplete.discard "ArrivalTime"
        |> DecodeComplete.discard "NextPasses"
        |> DecodeComplete.discard "Direction"
        |> DecodeComplete.discard "IsDestination"
        |> DecodeComplete.discard "IsStarted"
        |> DecodeComplete.discard "LineType"
        |> DecodeComplete.discard "Note"
        |> DecodeComplete.discard "Platform"
        |> DecodeComplete.discard "Race"
        |> DecodeComplete.discard "Spare2"
        |> DecodeComplete.discard "StopCode"
        |> DecodeComplete.discard "TransitType"
        |> DecodeComplete.complete
    )
        |> Json.Decode.list
        |> Json.Decode.map (List.filterMap identity)


angleDecoder : Decoder Angle
angleDecoder =
    Json.Decode.map Angle.degrees Json.Decode.float


busStopsDecoder : Decoder (List StopInfo)
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
                            ++ escape constant
                            ++ ", found "
                            ++ escape raw
                        )
            )


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)


busStopDecoder : Decoder StopInfo
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


propertiesDecoder :
    Decoder
        { name : String
        , code : Id Stop
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
        |> DecodeComplete.required "code" idDecoder
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


idDecoder : Decoder (Id a)
idDecoder =
    Json.Decode.map Id.fromString Json.Decode.string
