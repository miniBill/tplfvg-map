module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), Stop, ToBackend(..), ToFrontend(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Http
import Lamdera exposing (ClientId, SessionId)
import RemoteData exposing (WebData)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , stops : WebData (List Stop)
    }


type alias BackendModel =
    { stops : WebData (List Stop) }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | Reload


type ToBackend
    = TBReload


type BackendMsg
    = GotStops (Result Http.Error (List Stop))
    | OnConnect SessionId ClientId


type alias Stop =
    {}


type ToFrontend
    = TFStops (Result Http.Error (List Stop))
