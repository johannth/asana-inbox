module Api exposing (..)

import Json.Decode as Decode
import Dict exposing (Dict)
import Http
import Types exposing (..)
import Date exposing (Date)
import Set


apiUrl : String -> String -> String
apiUrl apiHost path =
    apiHost ++ path
