module Types exposing (..)

import Dict exposing (Dict)
import Http
import Date exposing (Date)
import Navigation


type alias Model =
    { apiHost : String
    , buildInfo : BuildInfo
    }


type Msg
    = Void
    | UrlChange Navigation.Location


emptyModel : Flags -> Model
emptyModel flags =
    { apiHost = flags.apiHost
    , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
    }



-- BUILD INFO


type alias Flags =
    { apiHost : String
    , buildVersion : String
    , buildTier : String
    , buildTime : String
    }


type alias BuildInfo =
    { version : String
    , time : String
    , tier : String
    }
