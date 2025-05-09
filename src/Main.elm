module Main exposing (main)

import Angle
import Api
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import FNV1a
import Frame2d
import Html exposing (Html, text)
import Http
import List.Extra
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Unitless)
import Rectangle2d
import RemoteData exposing (RemoteData(..), WebData)
import Svg
import Svg.Attributes
import Svg.Attributes.Extra
import Types exposing (Point, Stop)


type alias Flags =
    {}


type alias Model =
    { stops : WebData (List Stop)
    }


type Msg
    = GotStops (Result Http.Error (List Stop))


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
    ( { stops = Loading }, Api.getStops GotStops )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStops result ->
            ( { model | stops = RemoteData.fromResult result }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ -- Html.button
          -- [ Html.Events.onClick Reload ]
          -- [ text "Reload" ],
          viewRemoteData viewHttpError viewStops model.stops
        ]
    }


viewStops : List Types.Stop -> Html Msg
viewStops items =
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

        stops =
            List.map viewStop items

        communes =
            items
                |> List.Extra.gatherEqualsBy .commune
                |> List.concatMap
                    (\( { commune } as head, tail ) ->
                        let
                            enlargedBounds : BoundingBox2d Unitless world
                            enlargedBounds =
                                getBounds (head :: tail)

                            ( width, height ) =
                                BoundingBox2d.dimensions enlargedBounds
                        in
                        [ Svg.rect
                            [ Svg.Attributes.Extra.x (BoundingBox2d.minX enlargedBounds)
                            , Svg.Attributes.Extra.y (BoundingBox2d.minY enlargedBounds)
                            , Svg.Attributes.Extra.width width
                            , Svg.Attributes.Extra.height height
                            , Svg.Attributes.stroke (communeToColor commune)
                            , Svg.Attributes.strokeWidth "0.004"
                            , Svg.Attributes.fill "transparent"
                            ]
                            []
                        , Svg.text_
                            [ Svg.Attributes.Extra.x (BoundingBox2d.minX enlargedBounds)
                            , Svg.Attributes.Extra.y
                                (BoundingBox2d.minY enlargedBounds
                                    |> Quantity.minus (Quantity.float 0.01)
                                )
                            , Svg.Attributes.fontSize "0.02"
                            , Svg.Attributes.fill (communeToColor commune)
                            ]
                            [ Svg.text commune ]
                        ]
                    )
    in
    Svg.svg [ Svg.Attributes.viewBox viewBox ]
        (stops ++ communes)


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


pointToCoordinates point =
    ( Angle.inDegrees point.longitude
    , -(Angle.inDegrees point.latitude)
    )


viewStop : Types.Stop -> Html Msg
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
        [ Svg.title [] [ Svg.text (stop.code ++ " - " ++ stop.name) ]
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
