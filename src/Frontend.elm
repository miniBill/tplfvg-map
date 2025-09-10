module Frontend exposing (app)

import Angle
import BoundingBox2d exposing (BoundingBox2d)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Data
import FNV1a
import Frame2d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Id
import Lamdera exposing (Key, Url)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Unitless)
import Rectangle2d
import SeqDict
import Svg exposing (Svg)
import Svg.Attributes
import Types exposing (Bus, FrontendModel, FrontendMsg(..), Point, ToFrontend(..))
import Url


app :
    { init :
        Url
        -> Key
        ->
            ( FrontendModel
            , Cmd FrontendMsg
            )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none


init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , buses = SeqDict.empty
      , dark = False
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        ToggleDark ->
            ( { model | dark = not model.dark }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFBuses buses ->
            ( { model
                | buses =
                    List.foldl
                        (\( vehicle, bus ) acc ->
                            SeqDict.insert vehicle bus acc
                        )
                        model.buses
                        buses
              }
            , Cmd.none
            )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.node "style"
            []
            [ Html.text """
                body {
                    margin: 0;
                    display : flex;
                    padding: 8px;
                    gap: 8px;
                    flex-direction: column;
                }
            """ ]
        , innerView model
        , Html.button
            [ Html.Events.onClick ToggleDark ]
            [ Html.text "Toggle theme" ]
        ]
    }


innerView : FrontendModel -> Html msg
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
                |> SeqDict.values
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
