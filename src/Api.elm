module Api exposing (getEndpoints, getStops)

import Angle exposing (Angle)
import DecodeComplete
import Http
import Id exposing (Environment, Id, Line, Stop)
import IdSet exposing (IdSet)
import Json.Decode exposing (Decoder)
import Json.Extra
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Match, Regex)
import Task exposing (Task)
import Types exposing (Point, Service(..), StopInfo)


getStops : (Result Http.Error (List StopInfo) -> msg) -> Cmd msg
getStops toMsg =
    Http.get
        { url =
            -- "https://tplfvg.it/services/bus-stops/all"
            "/services/bus-stops/all.json"
        , expect = Http.expectJson toMsg busStopsDecoder
        }


getEndpoints : (Result Http.Error (IdSet Stop) -> msg) -> Id Stop -> Cmd msg
getEndpoints toMsg rootId =
    let
        step : IdSet Stop -> List (Id Stop) -> Task Http.Error (IdSet Stop)
        step acc queue =
            case queue of
                stopId :: tail ->
                    getLinesPassingByStop stopId
                        |> Task.andThen
                            (\lines ->
                                lines
                                    |> List.map getEndpointsForLine
                                    |> Task.sequence
                                    |> Task.map List.concat
                            )
                        |> Task.andThen (\newIds -> step (IdSet.insertAll newIds acc) tail)

                [] ->
                    Task.succeed acc
    in
    step IdSet.empty [ rootId ]
        |> Task.attempt toMsg


getLinesPassingByStop : Id Stop -> Task Http.Error (List ( Id Environment, Id Line ))
getLinesPassingByStop stopId =
    Http.task
        { url =
            -- "https://tplfvg.it/it/il-viaggio/costruisci-il-tuo-orario/?bus_stop=" ++ Id.toString stopId ++ "&search-lines-by-bus-stops"
            "/it/il-viaggio/costruisci-il-tuo-orario/?bus_stop=" ++ Id.toString stopId ++ "&search-lines-by-bus-stops"
        , resolver =
            Http.stringResolver
                (\res ->
                    res
                        |> generalResolver
                        |> Result.map parseLines
                )
        , method = "GET"
        , timeout = Nothing
        , headers = [ Http.header "X-Requested-With" "XMLHttpRequest" ]
        , body = Http.emptyBody
        }


getEndpointsForLine : ( Id Environment, Id Line ) -> Task Http.Error (List (Id Stop))
getEndpointsForLine ( environment, line ) =
    Http.task
        { url =
            -- "https://tplfvg.it/services/timetables/full/?code=" ++ Id.toString line ++ "&lang=it&env=" ++ Id.toString environment
            "/services/timetables/full/?code=" ++ Id.toString line ++ "&lang=it&env=" ++ Id.toString environment
        , resolver =
            Http.stringResolver
                (\res ->
                    res
                        |> generalResolver
                        |> Result.map parseEndpoints
                )
        , method = "GET"
        , timeout = Nothing
        , headers = [ Http.header "X-Requested-With" "XMLHttpRequest" ]
        , body = Http.emptyBody
        }


generalResolver : Http.Response a -> Result Http.Error a
generalResolver response =
    case response of
        Http.BadUrl_ url ->
            Http.BadUrl url |> Err

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Http.BadStatus metadata.statusCode |> Err

        Http.GoodStatus_ _ body ->
            Ok body


lineRegex : Regex
lineRegex =
    Regex.fromString "\"environment_code\": \"([A-Z0-9]+)\", \"group_code\": \"[A-Z0-9]+\", \"guideline_code\": \"([A-Z0-9]+)\""
        |> Maybe.withDefault Regex.never


parseLines : String -> List ( Id Environment, Id Line )
parseLines raw =
    Regex.find lineRegex raw
        |> List.filterMap
            (\match ->
                case match.submatches of
                    [ Just environmentCode, Just guidelineCode ] ->
                        Just ( Id.fromString environmentCode, Id.fromString guidelineCode )

                    _ ->
                        Nothing
            )


endpointRegex : Regex
endpointRegex =
    Regex.fromString "<strong>([A-Z0-9]+)</strong>"
        |> Maybe.withDefault Regex.never


parseEndpoints : String -> List (Id Stop)
parseEndpoints raw =
    let
        matches : List Match
        matches =
            Regex.find endpointRegex raw

        extract : Maybe Regex.Match -> List (Id stop)
        extract match =
            case Maybe.map .submatches match of
                Just [ Just stopCode ] ->
                    [ Id.fromString stopCode ]

                _ ->
                    []
    in
    extract (List.head matches) ++ extract (List.Extra.last matches)


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
                            ++ Json.Extra.escape constant
                            ++ ", found "
                            ++ Json.Extra.escape raw
                        )
            )


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


idDecoder : Decoder (Id a)
idDecoder =
    Json.Decode.map Id.fromString Json.Decode.string


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
