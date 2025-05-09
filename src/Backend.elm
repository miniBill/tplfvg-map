module Backend exposing (app)

import Api
import Lamdera exposing (ClientId, SessionId)
import RemoteData exposing (RemoteData(..))
import Types exposing (BackendModel, BackendMsg(..), ToBackend(..), ToFrontend(..))


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { stops = NotAsked }, Cmd.none )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        GotStops result ->
            ( { model | stops = RemoteData.fromResult result }
            , Lamdera.broadcast (TFStops result)
            )

        OnConnect _ clientId ->
            ( model
            , case model.stops of
                RemoteData.NotAsked ->
                    Cmd.none

                RemoteData.Loading ->
                    Cmd.none

                RemoteData.Success success ->
                    Lamdera.sendToFrontend clientId (TFStops (Ok success))

                RemoteData.Failure failure ->
                    Lamdera.sendToFrontend clientId (TFStops (Err failure))
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        TBReload ->
            ( { model | stops = Loading }, Api.getStops GotStops )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Lamdera.onConnect OnConnect
