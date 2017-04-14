module State exposing (..)

import Types exposing (..)
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)
import Dict exposing (Dict)
import Dom exposing (focus)
import Task


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        initialModel =
            emptyModel flags

        initialCommands =
            []
    in
        initialModel ! initialCommands


placeDroppedTask : TaskListIndex -> TaskListIndex -> Array ( AsanaTaskCategory, String ) -> Array ( AsanaTaskCategory, String )
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


insertTaskAfterIndex : TaskListIndex -> String -> Array ( AsanaTaskCategory, String ) -> Array ( AsanaTaskCategory, String )
insertTaskAfterIndex ( taskCategory, index ) taskId taskList =
    let
        firstHalf =
            (Array.slice 0 (index + 1) taskList) |> Array.toList

        secondHalf =
            (Array.slice (index + 1) (Array.length taskList) taskList) |> Array.toList
    in
        Array.fromList (firstHalf ++ [ ( taskCategory, taskId ) ] ++ secondHalf)


toggleExpandedState : AsanaTaskCategory -> ExpandedState -> ExpandedState
toggleExpandedState taskCategory currentState =
    case taskCategory of
        Today ->
            { currentState | today = not currentState.today }

        New ->
            { currentState | new = not currentState.new }

        Upcoming ->
            { currentState | upcoming = not currentState.upcoming }

        Later ->
            { currentState | later = not currentState.later }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Void ->
            model ! []

        FocusResult _ ->
            model ! []

        UrlChange newLocation ->
            model ! []

        ToggleExpanded taskCategory ->
            { model | expanded = toggleExpandedState taskCategory model.expanded } ! []

        CompleteTask completedTaskId ->
            { model | taskList = Array.filter (\( _, taskId ) -> taskId /= completedTaskId) model.taskList } ! []

        EditTaskTitle taskId title ->
            case Dict.get taskId model.tasks of
                Just task ->
                    let
                        updatedTask =
                            { task | title = title }
                    in
                        { model | tasks = Dict.insert taskId updatedTask model.tasks } ! []

                Nothing ->
                    model ! []

        AddNewTask index ->
            let
                tempId =
                    toString (Array.length model.taskList)

                task =
                    AsanaTask tempId Nothing "" Nothing

                taskList =
                    insertTaskAfterIndex index task.id model.taskList
            in
                { model | tasks = Dict.insert task.id task model.tasks, taskList = taskList } ! [ Task.attempt FocusResult (focus tempId) ]

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
