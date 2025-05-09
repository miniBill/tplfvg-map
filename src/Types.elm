module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), Point, Service(..), StopInfo, ToBackend(..), ToFrontend(..))

import Angle exposing (Angle)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Id exposing (Id, Stop)
import Lamdera exposing (ClientId, SessionId)
import RemoteData exposing (WebData)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , stops : WebData (List StopInfo)
    }


type alias BackendModel =
    { stops : WebData (List StopInfo) }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | Reload


type ToBackend
    = TBReload


type BackendMsg
    = GotStops (Result Http.Error (List StopInfo))
    | OnConnect SessionId ClientId


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


type ToFrontend
    = TFStops (Result Http.Error (List StopInfo))
