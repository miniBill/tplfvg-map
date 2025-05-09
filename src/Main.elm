module Main exposing (main)

import Api
import Browser
import Html exposing (Html, text)
import Html.Attributes
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (Stop)


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
          viewStops model.stops
        ]
    }


viewStops : RemoteData.WebData (List Types.Stop) -> Html Msg
viewStops data =
    viewRemoteData viewHttpError (viewList viewStop) data


viewStop : Types.Stop -> Html Msg
viewStop stop =
    text "TODO"


viewList : (a -> Html msg) -> List a -> Html msg
viewList viewItem items =
    items
        |> List.map viewItem
        |> Html.div
            [ Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "row"
            , Html.Attributes.style "flex-wrap" "wrap"
            ]


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
