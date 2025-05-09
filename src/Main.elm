module Main exposing (main)

import Angle
import Api
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import FNV1a
import Frame2d
import Html exposing (Html, text)
import Html.Attributes
import Http
import Id exposing (Stop)
import IdSet exposing (IdSet)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Unitless)
import Rectangle2d
import RemoteData exposing (RemoteData(..), WebData)
import Svg exposing (Svg)
import Svg.Attributes
import Types exposing (Point, StopInfo)


type alias Flags =
    {}


type alias Model =
    { stops : WebData (List StopInfo)
    , endpoints : WebData (IdSet Stop)
    }


type Msg
    = GotStops (Result Http.Error (List StopInfo))
    | GotEndpoints (Result Http.Error (IdSet Stop))


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
    ( { stops = Loading
      , endpoints = Loading
      }
    , Cmd.batch
        [ Api.getStops GotStops
        , Api.getEndpoints GotEndpoints (Id.fromString "70101")
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStops result ->
            ( { model | stops = RemoteData.fromResult result }, Cmd.none )

        GotEndpoints result ->
            ( { model | endpoints = RemoteData.fromResult result }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ viewRemoteData viewHttpError (viewStops model.endpoints) model.stops
        , viewRemoteData viewHttpError (\_ -> Html.text "") model.endpoints
        ]
    }


viewStops : WebData (IdSet Stop) -> List Types.StopInfo -> Html msg
viewStops endpoints items =
    let
        bounds : BoundingBox2d Unitless world
        bounds =
            getBounds items

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
            List.map viewStop items

        endpointsViews : List (Svg msg)
        endpointsViews =
            case endpoints of
                RemoteData.Success ids ->
                    items
                        |> List.filter (\stop -> IdSet.member stop.code ids)
                        |> List.map viewEndpoint

                RemoteData.Loading ->
                    []

                RemoteData.NotAsked ->
                    []

                RemoteData.Failure _ ->
                    []
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBox
        , Html.Attributes.style "height" "auto"
        , Html.Attributes.style "width" "100%"
        ]
        (stops ++ endpointsViews)


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


communeToColor : String -> String
communeToColor commune =
    "oklch(50% 0.09 " ++ (FNV1a.hash commune |> modBy 360 |> String.fromInt) ++ ")"


viewHttpError : Http.Error -> Html msg
viewHttpError err =
    let
        msg : String
        msg =
            case err of
                Http.BadBody reason ->
                    "Bad body: " ++ reason

                Http.BadUrl _ ->
                    "Bad url"

                Http.Timeout ->
                    "Timeout"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus status ->
                    "Bad status " ++ String.fromInt status
    in
    Html.text msg


viewRemoteData : (e -> Html msg) -> (a -> Html msg) -> RemoteData e a -> Html msg
viewRemoteData onError onSuccess data =
    case data of
        RemoteData.Success success ->
            onSuccess success

        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            text "Loading"

        RemoteData.Failure err ->
            onError err


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
