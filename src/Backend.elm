module Backend exposing (app)

import Api
import Cmd.Extra
import Duration
import Fifo exposing (Fifo)
import Http
import Id exposing (Id, Stop)
import Lamdera exposing (ClientId, SessionId)
import SeqDict
import SeqSet exposing (SeqSet)
import SetFifo exposing (SetFifo)
import Task exposing (Task)
import Time
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
    ( { buses = SeqDict.empty
      , pending = SeqSet.empty
      , stops = []
      , fastQueue = SetFifo.empty
      , slowQueue = SetFifo.empty
      }
    , Task.attempt GotStops Api.getStops
    )


maxPending : number
maxPending =
    10


processQueue : BackendModel -> ( BackendModel, Cmd BackendMsg )
processQueue model =
    if SeqSet.size model.pending < maxPending then
        let
            ( toStart, tail ) =
                let
                    size : Int
                    size =
                        maxPending - SeqSet.size model.pending

                    go : Int -> SeqSet (Id Stop) -> SetFifo (Id Stop) -> ( SeqSet (Id Stop), SetFifo (Id Stop) )
                    go n acc queue =
                        if n >= size then
                            ( acc, queue )

                        else
                            case SetFifo.dequeue queue of
                                Nothing ->
                                    ( acc, queue )

                                Just ( head, rest ) ->
                                    if
                                        SeqSet.member head model.pending
                                            || SeqSet.member head acc
                                    then
                                        go n acc rest

                                    else
                                        go (n + 1) (SeqSet.insert head acc) rest
                in
                go 0 SeqSet.empty model.fastQueue

            newCmds : List (Cmd BackendMsg)
            newCmds =
                toStart
                    |> SeqSet.toList
                    |> List.indexedMap
                        (\i ->
                            Api.getBusesForStopId
                                (Duration.milliseconds (1000 + 100 * toFloat i))
                        )

            _ =
                SeqSet.toList toStart
                    |> List.map (\id -> Id.toString id)
                    |> String.join ", "
                    |> Lamdera.log "Queueing new requests"
        in
        ( { model
            | pending = SeqSet.union toStart model.pending
            , fastQueue = tail
          }
        , Cmd.batch newCmds
        )

    else
        let
            _ =
                Lamdera.log "Queue is full" ()
        in
        ( model, Cmd.none )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    (case msg of
        OnConnect _ clientId ->
            ( model
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId (TFStops model.stops)
                , Lamdera.sendToFrontend clientId (TFBuses (SeqDict.toList model.buses))
                ]
            )

        GotBusesFromStop stop result ->
            case result of
                Ok buses ->
                    let
                        ( fastQueue, slowQueue ) =
                            if List.isEmpty buses then
                                ( model.fastQueue
                                , SetFifo.enqueue stop model.slowQueue
                                )

                            else
                                ( SetFifo.enqueue stop model.fastQueue
                                , model.slowQueue
                                )
                    in
                    ( { model
                        | pending = SeqSet.remove stop model.pending
                        , buses =
                            List.foldl
                                (\( vehicle, bus ) acc ->
                                    SeqDict.insert vehicle bus acc
                                )
                                model.buses
                                buses
                        , fastQueue = fastQueue
                        , slowQueue = slowQueue
                      }
                    , Lamdera.broadcast (TFBuses (SeqDict.toList model.buses))
                    )

                Err e ->
                    let
                        _ =
                            Lamdera.log ("Error getting buses: " ++ errorToString e) ()
                    in
                    { model
                        | pending = SeqSet.remove stop model.pending
                        , slowQueue = SetFifo.enqueue stop model.slowQueue
                    }
                        |> Cmd.Extra.pure

        Tick _ ->
            case SetFifo.dequeue model.slowQueue of
                Nothing ->
                    model
                        |> Cmd.Extra.pure

                Just ( head, tail ) ->
                    { model
                        | fastQueue = SetFifo.enqueue head model.fastQueue
                        , slowQueue = tail
                    }
                        |> Cmd.Extra.pure

        GotStops (Err e) ->
            let
                _ =
                    Lamdera.log ("Error getting stops: " ++ errorToString e) ()
            in
            model
                |> Cmd.Extra.pure

        GotStops (Ok stops) ->
            ( { model
                | stops = stops
              }
            , Cmd.batch
                [ Lamdera.broadcast (TFStops stops)
                , Api.getEndpoints (List.map .code stops)
                    |> Task.attempt GotEndpoints
                ]
            )

        GotEndpoints (Err e) ->
            let
                _ =
                    Lamdera.log ("Error getting endpoints: " ++ errorToString e) ()
            in
            model
                |> Cmd.Extra.pure

        GotEndpoints (Ok endpoints) ->
            { model | fastQueue = SetFifo.fromSeqSet endpoints }
                |> Cmd.Extra.pure
    )
        |> Cmd.Extra.andThen processQueue


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout"

        Http.BadUrl u ->
            "Bad url: " ++ u

        Http.NetworkError ->
            "Network error"

        Http.BadStatus s ->
            "Bad status: " ++ String.fromInt s

        Http.BadBody b ->
            "Bad body: " ++ b


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        TBReloadBusStops ->
            ( model, Task.attempt GotStops Api.getStops )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect OnConnect
        , Time.every 10000 Tick
        ]
