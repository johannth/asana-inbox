module State exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)
import Api
import Types exposing (..)
import Navigation
import Http
import Set
import Task
import Time


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        initialModel =
            emptyModel flags

        initialCommands =
            []
    in
        initialModel ! initialCommands


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Void ->
            model ! []

        UrlChange newLocation ->
            model ! []
