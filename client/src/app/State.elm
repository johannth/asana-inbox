module State exposing (..)

import Types exposing (..)
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        initialModel =
            emptyModel flags

        initialCommands =
            []
    in
        initialModel ! initialCommands


placeDroppedTask : DragDropIndex -> DragDropIndex -> Array ( TaskCategory, String ) -> Array ( TaskCategory, String )
placeDroppedTask ( dragCategory, dragIndex ) ( dropCategory, dropIndex ) tasks =
    case Array.get dragIndex tasks of
        Just ( _, dragTaskId ) ->
            let
                filterDragged =
                    Array.filter (\( _, taskId ) -> taskId /= dragTaskId)

                firstHalf =
                    (Array.slice 0 dropIndex tasks) |> filterDragged |> Array.toList

                secondHalf =
                    (Array.slice dropIndex (Array.length tasks) tasks) |> filterDragged |> Array.toList
            in
                Array.fromList (firstHalf ++ [ ( dropCategory, dragTaskId ) ] ++ secondHalf)

        Nothing ->
            tasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Void ->
            model ! []

        UrlChange newLocation ->
            model ! []

        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop

                taskList =
                    case result of
                        Just ( dragId, dropId ) ->
                            placeDroppedTask dragId dropId model.taskList

                        Nothing ->
                            model.taskList
            in
                { model
                    | dragDrop = model_
                    , taskList = taskList
                }
                    ! []
