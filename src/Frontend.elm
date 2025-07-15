module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Http
import Lamdera exposing (Key, Url)
import RemoteData exposing (RemoteData)
import Types exposing (FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))
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
init url key =
    ( { key = key
      , stops = RemoteData.Loading
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

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Reload ->
            ( model, Lamdera.sendToBackend TBReload )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TFStops result ->
            ( { model | stops = RemoteData.fromResult result }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.button
            [ Html.Events.onClick Reload ]
            [ Html.text "Reload" ]
        , viewStops model.stops
        ]
    }


viewStops : RemoteData.WebData (List Types.StopInfo) -> Html FrontendMsg
viewStops data =
    viewRemoteData viewHttpError (viewList viewStop) data


viewStop : Types.StopInfo -> Html FrontendMsg
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
