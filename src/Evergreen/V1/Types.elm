module Evergreen.V1.Types exposing (..)

import Angle
import Evergreen.V1.Id
import Http
import Lamdera
import SeqDict
import SeqSet
import Time


type alias Point =
    { latitude : Angle.Angle
    , longitude : Angle.Angle
    }


type alias Bus =
    { name : String
    , line : Evergreen.V1.Id.Id Evergreen.V1.Id.Line
    , coordinates : Point
    , stop : Evergreen.V1.Id.Id Evergreen.V1.Id.Stop
    }


type alias FrontendModel =
    { key : Lamdera.Key
    , buses : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.Id.Vehicle) Bus
    , dark : Bool
    }


type alias BackendModel =
    { buses : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.Id.Vehicle) Bus
    , pending : SeqSet.SeqSet (Evergreen.V1.Id.Id Evergreen.V1.Id.Stop)
    , fastQueue : List (Evergreen.V1.Id.Id Evergreen.V1.Id.Stop)
    , slowQueue : List (Evergreen.V1.Id.Id Evergreen.V1.Id.Stop)
    }


type FrontendMsg
    = UrlClicked Lamdera.UrlRequest
    | UrlChanged Lamdera.Url
    | ToggleDark


type ToBackend
    = TBNoop


type BackendMsg
    = OnConnect Lamdera.SessionId Lamdera.ClientId
    | GotBusesFromStop (Evergreen.V1.Id.Id Evergreen.V1.Id.Stop) (Result Http.Error (List ( Evergreen.V1.Id.Id Evergreen.V1.Id.Vehicle, Bus )))
    | Tick Time.Posix


type ToFrontend
    = TFBuses (List ( Evergreen.V1.Id.Id Evergreen.V1.Id.Vehicle, Bus ))
