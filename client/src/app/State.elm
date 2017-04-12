module State exposing (..)

import Types exposing (..)
import Navigation


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
