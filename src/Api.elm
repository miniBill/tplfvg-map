module Api exposing (getBusesForStopId)

import Angle exposing (Angle)
import DecodeComplete
import Duration exposing (Duration)
import Http
import Id exposing (Id, Stop, Vehicle)
import Json.Decode
import Process
import Quantity
import Task
import Types exposing (BackendMsg(..), Bus)


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


jsonResolver : Json.Decode.Decoder a -> Http.Resolver Http.Error a
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


busesDecoder : Id Stop -> Json.Decode.Decoder (List ( Id Vehicle, Bus ))
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


angleDecoder : Json.Decode.Decoder Angle
angleDecoder =
    Json.Decode.map Angle.degrees Json.Decode.float
