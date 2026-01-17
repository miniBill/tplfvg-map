module Types exposing (BackendModel, BackendMsg(..), Bus, FrontendModel, FrontendMsg(..), Point, Service(..), StopInfo, ToBackend(..), ToFrontend(..))

import Angle exposing (Angle)
import Fifo exposing (Fifo)
import Http
import Id exposing (Id, Line, Stop, Vehicle)
import Lamdera exposing (ClientId, SessionId, Url, UrlRequest)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Time


type alias FrontendModel =
    { key : Lamdera.Key
    , buses : SeqDict (Id Vehicle) Bus
    , stops : List StopInfo
    , dark : Bool
    }


type alias BackendModel =
    { buses : SeqDict (Id Vehicle) Bus

    -- Queues
    , pending : SeqSet (Id Stop)
    , fastQueue : Fifo (Id Stop)
    , slowQueue : Fifo (Id Stop)
    , stops : List StopInfo
    }


type alias Bus =
    { name : String
    , line : Id Line
    , coordinates : Point
    , stop : Id Stop
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ToggleDark


type ToBackend
    = TBNoop


type BackendMsg
    = OnConnect SessionId ClientId
    | GotBusesFromStop (Id Stop) (Result Http.Error (List ( Id Vehicle, Bus )))
    | Tick Time.Posix
    | GotStops (Result Http.Error (List StopInfo))


type ToFrontend
    = TFStops (List StopInfo)
    | TFBuses (List ( Id Vehicle, Bus ))


type alias StopInfo =
    { name : String
    , code : Id Stop
    , commune : String
    , coordinates : Point
    , services : List Service
    }


type alias Point =
    { latitude : Angle
    , longitude : Angle
    }


type Service
    = Urban
    | Maritime
    | ExtraUrban
