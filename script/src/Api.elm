module Api exposing (getEndpoints, getStops)

import Angle exposing (Angle)
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import CachedHttp
import DecodeComplete
import FatalError exposing (FatalError)
import Id exposing (Environment, Id, Line, Stop)
import IdSet exposing (IdSet)
import Json.Decode exposing (Decoder)
import Json.Extra
import List.Extra
import Pages.Script as Script
import Regex exposing (Match, Regex)
import Result.Extra
import Types exposing (Point, Service(..), StopInfo)


getStops : BackendTask FatalError (List StopInfo)
getStops =
    CachedHttp.getJson
        "https://tplfvg.it/services/bus-stops/all"
        busStopsDecoder


getEndpoints : List (Id Stop) -> BackendTask FatalError (IdSet Stop)
getEndpoints initialQueue =
    let
        step : IdSet Stop -> IdSet Line -> List (Id Stop) -> BackendTask FatalError (IdSet Stop)
        step acc failingLines queue =
            let
                queueLength : Int
                queueLength =
                    List.length queue
            in
            Do.do
                (if (queueLength |> modBy 10) == 0 then
                    Script.log ("Queue size: " ++ String.fromInt queueLength)

                 else
                    BackendTask.succeed ()
                )
            <| \_ ->
            case ( List.take 11 queue, List.drop 11 queue ) of
                ( [], _ ) ->
                    BackendTask.succeed acc

                ( head, tail ) ->
                    head
                        |> List.map
                            (\stopId ->
                                getLinesPassingByStop stopId
                                    |> BackendTask.andThen
                                        (\lines ->
                                            lines
                                                |> List.Extra.removeWhen (\( _, line ) -> IdSet.member line failingLines)
                                                |> List.map getEndpointsForLine
                                                |> List.Extra.greedyGroupsOf 10
                                                |> List.map BackendTask.combine
                                                |> BackendTask.sequence
                                                |> BackendTask.map List.concat
                                        )
                            )
                        |> BackendTask.combine
                        |> BackendTask.map List.concat
                        |> BackendTask.andThen
                            (\results ->
                                let
                                    ( successes, failures ) =
                                        Result.Extra.partition results

                                    newIds : List (Id Stop)
                                    newIds =
                                        List.concat successes

                                    newAcc : IdSet Stop
                                    newAcc =
                                        IdSet.insertAll newIds acc
                                in
                                step
                                    newAcc
                                    (IdSet.insertAll failures failingLines)
                                    ((newIds ++ tail)
                                        |> List.Extra.removeWhen
                                            (\id ->
                                                IdSet.member id newAcc
                                            )
                                    )
                            )
    in
    step IdSet.empty
        (IdSet.fromList
            [ Id.fromString "U98DA"
            , Id.fromString "U232"
            , Id.fromString "U98DR"
            , Id.fromString "U231"
            ]
        )
        initialQueue


getLinesPassingByStop : Id Stop -> BackendTask FatalError (List ( Id Environment, Id Line ))
getLinesPassingByStop stopId =
    CachedHttp.getString
        ("https://tplfvg.it/it/il-viaggio/costruisci-il-tuo-orario/?bus_stop="
            ++ Id.toString stopId
            ++ "&search-lines-by-bus-stops"
        )
        |> BackendTask.map parseLines


getEndpointsForLine : ( Id Environment, Id Line ) -> BackendTask FatalError (Result (Id Line) (List (Id Stop)))
getEndpointsForLine ( environment, line ) =
    CachedHttp.getString
        ("https://tplfvg.it/services/timetables/full/?code=" ++ Id.toString line ++ "&lang=it&env=" ++ Id.toString environment)
        |> BackendTask.map parseEndpoints
        |> BackendTask.map Ok
        |> BackendTask.onError (\_ -> BackendTask.succeed (Err line))


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
