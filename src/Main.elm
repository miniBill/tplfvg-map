port module Main exposing (main)

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Data
import DecodeComplete
import Duration exposing (Duration)
import FNV1a
import Frame2d
import Html exposing (Html)
import Html.Attributes
import Http
import Id exposing (Id, Stop, Vehicle)
import IdDict exposing (IdDict)
import IdSet exposing (IdSet)
import Json.Decode
import Platform.Cmd as Cmd
import Point2d exposing (Point2d)
import Process
import Quantity exposing (Quantity, Unitless)
import Rectangle2d
import RemoteData exposing (RemoteData(..))
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Time
import Types exposing (Point)


port send : Json.Decode.Value -> Cmd msg


port receive : (Json.Decode.Value -> msg) -> Sub msg


type alias Flags =
    {}


type alias Bus =
    { name : String
    , coordinates : Point
    }


type alias Model =
    { -- tasks : ConcurrentTask.Extra.Pool Msg
      pending : IdSet Stop
    , initialQueue : List (Id Stop)
    , fastQueue : List (Id Stop)
    , slowQueue : List (Id Stop)
    , buses : IdDict Vehicle Bus
    }


type Msg
    = -- | OnProgress ( ConcurrentTask.Extra.Pool Msg, Cmd Msg )
      -- | OnUnexpected Task.UnexpectedError
      GotBusesFromStop (Id Stop) (Result Http.Error (List ( Id Vehicle, Bus )))
    | Tick Time.Posix


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { -- tasks = Task.pool
        pending = IdSet.empty
      , initialQueue = Data.endpoints
      , fastQueue = []
      , slowQueue = []
      , buses = IdDict.empty
      }
    , Cmd.none
    )
        |> processQueue


maxPending : number
maxPending =
    10


processQueue : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processQueue ( model, cmd ) =
    if IdSet.size model.pending < maxPending then
        let
            size : Int
            size =
                maxPending - IdSet.size model.pending

            ( toStart, tail ) =
                let
                    go n acc queue =
                        if n >= size then
                            ( acc, queue )

                        else
                            case queue of
                                [] ->
                                    ( acc, queue )

                                head :: rest ->
                                    if
                                        IdSet.member head model.pending
                                            || IdSet.member head acc
                                    then
                                        go n acc rest

                                    else
                                        go (n + 1) (IdSet.insert head acc) rest
                in
                if List.isEmpty model.initialQueue then
                    go 0 IdSet.empty model.fastQueue

                else
                    go 0 IdSet.empty model.initialQueue

            newCmds : List (Cmd Msg)
            newCmds =
                toStart
                    |> IdSet.toList
                    |> List.indexedMap
                        (\i ->
                            getBusesForStopId
                                (if List.isEmpty model.initialQueue then
                                    Duration.milliseconds (1000 + 100 * toFloat i)

                                 else
                                    Quantity.zero
                                )
                        )
        in
        ( { model
            | pending = IdSet.union toStart model.pending
            , initialQueue =
                if List.isEmpty model.initialQueue then
                    model.initialQueue

                else
                    tail
            , fastQueue =
                if List.isEmpty model.initialQueue then
                    tail

                else
                    model.fastQueue
          }
        , Cmd.batch (cmd :: newCmds)
        )

    else
        ( model, cmd )


getBusesForStopId : Duration -> Id Stop -> Cmd Msg
getBusesForStopId sleepTime stopId =
    Process.sleep (Duration.inMilliseconds sleepTime)
        |> Task.andThen
            (\_ ->
                Http.task
                    { url =
                        "https://realtime.tplfvg.it/API/v1.0/polemonitor/mrcruns?StopCode=" ++ Id.toString stopId ++ "&IsUrban=true"
                    , method = "GET"
                    , timeout = Nothing
                    , resolver = jsonResolver busesDecoder
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


busesDecoder : Json.Decode.Decoder (List ( Id Vehicle, Bus ))
busesDecoder =
    (DecodeComplete.object
        (\departure destination latitude longitude vehicle ->
            if
                (longitude == Quantity.zero)
                    && (latitude == Quantity.zero)
            then
                Nothing

            else
                ( vehicle
                , { name = departure ++ " => " ++ destination
                  , coordinates =
                        { latitude = latitude
                        , longitude = longitude
                        }
                  }
                )
                    |> Just
        )
        |> DecodeComplete.discard "Line"
        |> DecodeComplete.discard "Time"
        |> DecodeComplete.required "Departure" Json.Decode.string
        |> DecodeComplete.discard "DepartureTime"
        |> DecodeComplete.required "Destination" Json.Decode.string
        |> DecodeComplete.discard "ArrivalTime"
        |> DecodeComplete.discard "NextPasses"
        |> DecodeComplete.required "Latitude" angleDecoder
        |> DecodeComplete.required "Longitude" angleDecoder
        |> DecodeComplete.required "Vehicle" (Json.Decode.map Id.fromString Json.Decode.string)
        |> DecodeComplete.discard "Direction"
        |> DecodeComplete.discard "IsDestination"
        |> DecodeComplete.discard "IsStarted"
        |> DecodeComplete.discard "LineCode"
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



-- attempt :
--     (Result error a -> Msg)
--     -> ConcurrentTask error a
--     -> ( Model, Cmd Msg )
--     -> ( Model, Cmd Msg )
-- attempt =
--     ConcurrentTask.Extra.attempt
--         { send = send
--         , onUnexpected = OnUnexpected
--         }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        -- OnProgress ( tasks, cmd ) ->
        --     ( { model | tasks = tasks }, cmd )
        -- OnUnexpected err ->
        --     let
        --         _ =
        --             Debug.log "Unexpected error" err
        --     in
        --     ( model, Cmd.none )
        Tick _ ->
            case model.slowQueue of
                [] ->
                    ( model, Cmd.none )

                head :: tail ->
                    ( { model
                        | fastQueue = head :: model.fastQueue
                        , slowQueue = tail
                      }
                    , Cmd.none
                    )

        GotBusesFromStop stop (Ok buses) ->
            ( { model
                | pending = IdSet.remove stop model.pending
                , buses =
                    List.foldl
                        (\( vehicle, bus ) acc ->
                            IdDict.insert vehicle bus acc
                        )
                        model.buses
                        buses
                , fastQueue =
                    if List.isEmpty buses then
                        model.fastQueue

                    else
                        stop :: model.fastQueue
                , slowQueue =
                    if List.isEmpty buses then
                        stop :: model.slowQueue

                    else
                        model.slowQueue
              }
            , Cmd.none
            )

        GotBusesFromStop stop (Err e) ->
            let
                _ =
                    Debug.log "Error getting runs" e
            in
            ( { model
                | pending = IdSet.remove stop model.pending
              }
            , Cmd.none
            )
    )
        |> processQueue


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        let
            pendingSize : Int
            pendingSize =
                IdSet.size model.pending

            initialLength : Int
            initialLength =
                List.length model.initialQueue

            fastLength : Int
            fastLength =
                List.length model.fastQueue

            slowLength : Int
            slowLength =
                List.length model.slowQueue
        in
        [ innerView model
        , Html.node "style"
            []
            [ Html.text """
                #stats th { text-align: left; }
                #stats td {
                    text-align: right;
                    font-variant-numeric: tabular-nums;
                }
            """ ]
        , Html.table [ Html.Attributes.id "stats" ]
            [ Html.tr []
                [ Html.th [] [ Html.text "Pending" ]
                , Html.td [] [ Html.text (String.fromInt pendingSize) ]
                ]
            , Html.tr []
                [ Html.th [] [ Html.text "Initial queue" ]
                , Html.td [] [ Html.text (String.fromInt initialLength) ]
                ]
            , Html.tr []
                [ Html.th [] [ Html.text "Fast queue" ]
                , Html.td [] [ Html.text (String.fromInt fastLength) ]
                ]
            , Html.tr []
                [ Html.th [] [ Html.text "Slow queue" ]
                , Html.td [] [ Html.text (String.fromInt slowLength) ]
                ]
            , Html.tr []
                [ Html.th [] [ Html.text "Total" ]
                , Html.td [] [ Html.text (String.fromInt (pendingSize + initialLength + fastLength + slowLength)) ]
                ]
            ]
        ]
    }


innerView : Model -> Html msg
innerView model =
    let
        endpoints =
            IdSet.fromList Data.endpoints

        bounds : BoundingBox2d Unitless world
        bounds =
            getBounds Data.stops

        viewBox : String
        viewBox =
            let
                ( width, height ) =
                    BoundingBox2d.dimensions bounds
            in
            [ BoundingBox2d.minX bounds
            , BoundingBox2d.minY bounds
            , width
            , height
            ]
                |> List.map (\q -> q |> Quantity.toFloat |> String.fromFloat)
                |> String.join " "

        stops : List (Svg msg)
        stops =
            List.map viewStop Data.stops

        endpointsViews : List (Svg msg)
        endpointsViews =
            Data.stops
                |> List.filter (\stop -> IdSet.member stop.code endpoints)
                |> List.map viewEndpoint

        buses : List (Svg msg)
        buses =
            model.buses
                |> IdDict.values
                |> List.map viewBus
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBox
        , Html.Attributes.style "height" "auto"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "max-height" "90vh"
        ]
        [ Svg.g [] stops
        , Svg.g [] endpointsViews
        , Svg.g [] buses
        ]


getBounds : List { a | coordinates : Point } -> BoundingBox2d Unitless world
getBounds items =
    let
        raw : { minx : Float, maxx : Float, miny : Float, maxy : Float }
        raw =
            List.foldl
                (\{ coordinates } prev ->
                    let
                        ( x, y ) =
                            pointToCoordinates coordinates
                    in
                    { minx = min prev.minx x
                    , maxx = max prev.maxx x
                    , miny = min prev.miny y
                    , maxy = max prev.maxy y
                    }
                )
                { minx = 1 / 0
                , maxx = -1 / 0
                , miny = 1 / 0
                , maxy = -1 / 0
                }
                items

        center : Point2d Unitless coordinates
        center =
            Point2d.unitless
                ((raw.minx + raw.maxx) / 2)
                ((raw.miny + raw.maxy) / 2)

        width : Quantity Float Unitless
        width =
            Quantity.float (max 0.02 (1.1 * (raw.maxx - raw.minx)))

        height : Quantity Float Unitless
        height =
            Quantity.float (max 0.02 (1.1 * (raw.maxy - raw.miny)))
    in
    Rectangle2d.centeredOn
        (Frame2d.atPoint center)
        ( width, height )
        |> Rectangle2d.boundingBox


pointToCoordinates : Point -> ( Float, Float )
pointToCoordinates point =
    ( Angle.inDegrees point.longitude
    , -(Angle.inDegrees point.latitude)
    )


viewStop : Types.StopInfo -> Html msg
viewStop stop =
    let
        ( cx, cy ) =
            pointToCoordinates stop.coordinates
    in
    Svg.circle
        [ Svg.Attributes.cx (String.fromFloat cx)
        , Svg.Attributes.cy (String.fromFloat cy)
        , Svg.Attributes.r "0.004"
        , Svg.Attributes.fill (communeToColor stop.commune)
        ]
        [ Svg.title [] [ Svg.text (Id.toString stop.code ++ " - " ++ stop.name) ]
        ]


viewEndpoint : Types.StopInfo -> Html msg
viewEndpoint stop =
    let
        ( cx, cy ) =
            pointToCoordinates stop.coordinates
    in
    Svg.circle
        [ Svg.Attributes.cx (String.fromFloat cx)
        , Svg.Attributes.cy (String.fromFloat cy)
        , Svg.Attributes.r "0.008"
        , Svg.Attributes.fill "red"
        , Svg.Attributes.strokeWidth "0.002"
        , Svg.Attributes.stroke "black"
        ]
        [ Svg.title [] [ Svg.text (Id.toString stop.code ++ " - " ++ stop.name) ]
        ]


viewBus : Bus -> Svg msg
viewBus bus =
    let
        ( cx, cy ) =
            pointToCoordinates bus.coordinates
    in
    Svg.circle
        [ Svg.Attributes.cx (String.fromFloat cx)
        , Svg.Attributes.cy (String.fromFloat cy)
        , Svg.Attributes.r "0.016"
        , Svg.Attributes.fill "green"
        , Svg.Attributes.strokeWidth "0.002"
        , Svg.Attributes.stroke "black"
        ]
        [ Svg.title [] [ Svg.text bus.name ]
        ]


communeToColor : String -> String
communeToColor commune =
    "oklch(50% 0.09 " ++ (FNV1a.hash commune |> modBy 360 |> String.fromInt) ++ ")"



-- viewHttpError : Http.Error -> Html msg
-- viewHttpError err =
--     let
--         msg : String
--         msg =
--             case err of
--                 Http.BadBody _ _ _ ->
--                     "Bad body"
--                 Http.BadUrl _ ->
--                     "Bad url"
--                 Http.Timeout ->
--                     "Timeout"
--                 Http.NetworkError ->
--                     "Network error"
--                 Http.BadStatus { statusCode } _ ->
--                     "Bad status " ++ String.fromInt statusCode
--     in
--     Html.text msg
-- viewRemoteData : (e -> Html msg) -> (a -> Html msg) -> RemoteData e a -> Html msg
-- viewRemoteData onError onSuccess data =
--     case data of
--         RemoteData.Success success ->
--             onSuccess success
--         RemoteData.NotAsked ->
--             text "Not asked"
--         RemoteData.Loading ->
--             text "Loading"
--         RemoteData.Failure err ->
--             onError err


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- Task.onProgress
    --     { send = send
    --     , receive = receive
    --     , onProgress = OnProgress
    --     }
    --     model.tasks
    Time.every 5000 Tick
