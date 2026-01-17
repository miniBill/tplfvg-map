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
      , fastQueue = Fifo.empty
      , slowQueue = Fifo.empty
      }
    , Api.getStops
    )


maxPending : number
maxPending =
    1


processQueue : BackendModel -> ( BackendModel, Cmd BackendMsg )
processQueue model =
    if SeqSet.size model.pending < maxPending then
        let
            ( toStart, tail ) =
                let
                    size : Int
                    size =
                        maxPending - SeqSet.size model.pending

                    go : Int -> SeqSet (Id Stop) -> Fifo (Id Stop) -> ( SeqSet (Id Stop), Fifo (Id Stop) )
                    go n acc queue =
                        if n >= size then
                            ( acc, queue )

                        else
                            case Fifo.remove queue of
                                ( Nothing, _ ) ->
                                    ( acc, queue )

                                ( Just head, rest ) ->
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
        in
        ( { model
            | pending = SeqSet.union toStart model.pending
            , fastQueue = tail
          }
        , Cmd.batch newCmds
        )

    else
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
                                , Fifo.insert stop model.slowQueue
                                )

                            else
                                ( Fifo.insert stop model.fastQueue
                                , model.slowQueue
                                )
                    in
                    { model
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
                        |> Cmd.Extra.pure

                Err e ->
                    let
                        _ =
                            Lamdera.log ("Error getting buses: " ++ errorToString e) ()
                    in
                    { model
                        | pending = SeqSet.remove stop model.pending
                        , slowQueue = Fifo.insert stop model.slowQueue
                    }
                        |> Cmd.Extra.pure

        Tick _ ->
            case Fifo.remove model.slowQueue of
                ( Nothing, _ ) ->
                    model
                        |> Cmd.Extra.pure

                ( Just head, tail ) ->
                    { model
                        | fastQueue = Fifo.insert head model.fastQueue
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
            { model
                | stops = stops
                , fastQueue = Fifo.fromList (List.map .code stops)
            }
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
            ( model, Api.getStops )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect OnConnect
        , Time.every 5000 Tick
        ]
