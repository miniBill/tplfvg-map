port module Main exposing (Bus, Flags, Model, Msg, main)

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
import Html.Events
import Http
import Id exposing (Id, Line, Stop, Vehicle)
import IdDict exposing (IdDict)
import IdSet exposing (IdSet)
import Json.Decode
import Platform.Cmd as Cmd
import Point2d exposing (Point2d)
import Process
import Quantity exposing (Quantity, Unitless)
import Rectangle2d
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
    , line : Id Line
    , coordinates : Point
    , stop : Id Stop
    }


type alias Model =
    { pending : IdSet Stop
    , initialQueue : List (Id Stop)
    , fastQueue : List (Id Stop)
    , slowQueue : List (Id Stop)
    , buses : IdDict Vehicle Bus
    , dark : Bool
    , pause : Bool
    }


type Msg
    = GotBusesFromStop (Id Stop) (Result Http.Error (List ( Id Vehicle, Bus )))
    | Tick Time.Posix
    | ToggleDark
    | TogglePause


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
    ( { pending = IdSet.empty
      , initialQueue = Data.endpoints
      , fastQueue = []
      , slowQueue = []
      , buses = IdDict.empty
      , dark = True
      , pause = False
      }
    , Cmd.none
    )
        |> processQueue


maxPending : number
maxPending =
    10


processQueue : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processQueue ( model, cmd ) =
    if IdSet.size model.pending < maxPending && not model.pause then
        let
            ( toStart, tail ) =
                let
                    size : Int
                    size =
                        maxPending - IdSet.size model.pending

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
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

        ToggleDark ->
            ( { model | dark = not model.dark }, Cmd.none )

        TogglePause ->
            ( { model | pause = not model.pause }, Cmd.none )
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
                body {
                    margin: 0;
                    display : flex;
                    padding: 8px;
                    gap: 8px;
                    flex-direction: column;
                }
                #stats th { text-align: left; }
                #stats td {
                    text-align: right;
                    font-variant-numeric: tabular-nums;
                }
            """ ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "gap" "8px"
            ]
            [ Html.button
                [ Html.Events.onClick ToggleDark ]
                [ Html.text "Toggle theme" ]
            , Html.button
                [ Html.Events.onClick TogglePause ]
                [ if model.pause then
                    Html.text "Resume updates"

                  else
                    Html.text "Pause updates"
                ]
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
        ]
    }


innerView : Model -> Html msg
innerView model =
    let
        -- endpoints =
        --     IdSet.fromList Data.endpoints
        viewBox : String
        viewBox =
            let
                bounds : BoundingBox2d Unitless world
                bounds =
                    getBounds Data.stops

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
            List.map (viewStop { dark = model.dark }) Data.stops

        buses : List (Svg msg)
        buses =
            model.buses
                |> IdDict.values
                |> List.map (viewBus { dark = model.dark })
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBox
        , Html.Attributes.style "height" "auto"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "max-height" "90vh"
        , Html.Attributes.style "background"
            (if model.dark then
                "black"

             else
                "white"
            )
        ]
        [ Svg.g [ Svg.Attributes.id "stops" ] stops
        , Svg.g [ Svg.Attributes.id "buses" ] buses
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
    ( 1000 * Angle.inDegrees point.longitude
    , 1000 * -(Angle.inDegrees point.latitude)
    )


viewStop : { dark : Bool } -> Types.StopInfo -> Html msg
viewStop { dark } stop =
    let
        ( cx, cy ) =
            pointToCoordinates stop.coordinates
    in
    Svg.a
        [ Svg.Attributes.xlinkHref ("https://realtime.tplfvg.it/?stopcode=" ++ Id.toString stop.code)
        ]
        [ Svg.path
            [ Svg.Attributes.d "M246,117.35,212.33,154.7a16,16,0,0,1-11.89,5.3H136v64a8,8,0,0,1-16,0V160H40a16,16,0,0,1-16-16V80A16,16,0,0,1,40,64h80V32a8,8,0,0,1,16,0V64h64.44a16,16,0,0,1,11.89,5.3L246,106.65A8,8,0,0,1,246,117.35Z"
            , Svg.Attributes.transform
                ("translate("
                    ++ String.fromFloat cx
                    ++ " "
                    ++ String.fromFloat cy
                    ++ ") scale(0.1) translate(-128 -128)"
                )
            , Svg.Attributes.fill (communeToColor stop.commune)
            , Svg.Attributes.strokeWidth "2"
            , Svg.Attributes.stroke
                (if dark then
                    "white"

                 else
                    "black"
                )
            ]
            [ Svg.title [] [ Svg.text (stop.commune ++ " - " ++ stop.name) ]
            ]
        ]


viewBus : { dark : Bool } -> Bus -> Svg msg
viewBus { dark } bus =
    let
        ( cx, cy ) =
            pointToCoordinates bus.coordinates
    in
    Svg.a
        [ Svg.Attributes.xlinkHref ("https://realtime.tplfvg.it/?stopcode=" ++ Id.toString bus.stop)
        ]
        [ Svg.path
            [ Svg.Attributes.d "M248,80v24a8,8,0,0,1-16,0V80a8,8,0,0,1,16,0ZM16,72a8,8,0,0,0-8,8v24a8,8,0,0,0,16,0V80A8,8,0,0,0,16,72Zm200-8V208a16,16,0,0,1-16,16H184a16,16,0,0,1-16-16v-8H88v8a16,16,0,0,1-16,16H56a16,16,0,0,1-16-16V64A32,32,0,0,1,72,32H184A32,32,0,0,1,216,64ZM104,148a12,12,0,1,0-12,12A12,12,0,0,0,104,148Zm72,0a12,12,0,1,0-12,12A12,12,0,0,0,176,148Zm24-76H56v40H200Z"
            , Svg.Attributes.transform
                ("translate("
                    ++ String.fromFloat cx
                    ++ " "
                    ++ String.fromFloat cy
                    ++ ") scale(0.2) translate(-128 -128)"
                )
            , Svg.Attributes.strokeWidth "4"
            , Svg.Attributes.fill
                (if dark then
                    "white"

                 else
                    "black"
                )
            , Svg.Attributes.stroke
                (if dark then
                    "black"

                 else
                    "white"
                )
            ]
            [ Svg.title []
                [ Svg.text
                    (if String.isEmpty bus.name then
                        Id.toString bus.line

                     else
                        Id.toString bus.line ++ " - " ++ bus.name
                    )
                ]
            ]
        ]


communeToColor : String -> String
communeToColor commune =
    "oklch(50% 0.09 " ++ (FNV1a.hash commune |> modBy 360 |> String.fromInt) ++ ")"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 Tick
