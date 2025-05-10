module Types exposing (Point, Service(..), StopInfo)

import Angle exposing (Angle)
import Id exposing (Id, Stop)



-- type alias FrontendModel =
--     { key : Key
--     , stops : WebData (List StopInfo)
--     }
-- type alias BackendModel =
--     { stops : WebData (List StopInfo) }
-- type FrontendMsg
--     = UrlClicked UrlRequest
--     | UrlChanged Url
--     | NoOpFrontendMsg
--     | Reload
-- type ToBackend
--     = TBReload
-- type BackendMsg
--     = GotStops (Result Http.Error (List StopInfo))
--     | OnConnect SessionId ClientId
-- type ToFrontend
--     = TFStops (Result Http.Error (List StopInfo))


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
