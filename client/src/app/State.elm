module State exposing (..)

import Types exposing (..)
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)
import Dict exposing (Dict)
import Dom exposing (focus)
import Task
import DatePicker exposing (defaultSettings)
import Date
import Api


titleInputId : String -> String
titleInputId taskId =
    "taskTitle" ++ taskId


initDatePickers : List AsanaTask -> ( Dict String DatePicker.DatePicker, List (Cmd Msg) )
initDatePickers tasks =
    let
        everyThing =
            tasks
                |> List.map
                    (\task ->
                        let
                            settings =
                                { defaultSettings | pickedDate = task.dueOn }

                            ( datePicker, datePickerFx ) =
                                DatePicker.init settings
                        in
                            ( task.id, datePicker, Cmd.map (ToDatePicker task.id) datePickerFx )
                    )

        datePickers =
            Dict.fromList (everyThing |> List.map (\( taskId, datePicker, _ ) -> ( taskId, datePicker )))

        effects =
            everyThing |> List.map (\( _, _, effect ) -> effect)
    in
        ( datePickers, effects )


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        initialModel =
            { apiHost = flags.apiHost
            , tasks = Dict.empty
            , taskList = Array.empty
            , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
            , dragDrop = DragDrop.init
            , expanded = { today = True, new = True, upcoming = False, later = False }
            , datePickers = Dict.empty
            }

        initialCommands =
            [ Api.getTasks initialModel.apiHost ]
    in
        initialModel ! initialCommands


placeDroppedTask : TaskListIndex -> TaskListIndex -> Array String -> Array String
placeDroppedTask ( dragCategory, dragIndex ) ( dropCategory, dropIndex ) tasks =
    case Array.get dragIndex tasks of
        Just dragTaskId ->
            let
                filterDragged =
                    Array.filter (\taskId -> taskId /= dragTaskId)

                firstHalf =
                    (Array.slice 0 dropIndex tasks) |> filterDragged |> Array.toList

                secondHalf =
                    (Array.slice dropIndex (Array.length tasks) tasks) |> filterDragged |> Array.toList
            in
                Array.fromList (firstHalf ++ [ dragTaskId ] ++ secondHalf)

        Nothing ->
            tasks


insertTaskAfterIndex : TaskListIndex -> String -> Array String -> Array String
insertTaskAfterIndex ( taskCategory, index ) taskId taskList =
    let
        firstHalf =
            (Array.slice 0 (index + 1) taskList) |> Array.toList

        secondHalf =
            (Array.slice (index + 1) (Array.length taskList) taskList) |> Array.toList
    in
        Array.fromList (firstHalf ++ [ taskId ] ++ secondHalf)


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

        LoadTasks (Err message) ->
            let
                x =
                    Debug.log "Error" message
            in
                model ! []

        LoadTasks (Ok newTasks) ->
            let
                taskList =
                    List.map .id newTasks

                tasks =
                    Dict.fromList (List.map (\task -> ( task.id, task )) newTasks)

                ( datePickers, datePickerFx ) =
                    initDatePickers (Dict.values tasks)
            in
                { model | taskList = Array.fromList taskList, tasks = tasks, datePickers = datePickers } ! datePickerFx

        ToggleExpanded taskCategory ->
            { model | expanded = toggleExpandedState taskCategory model.expanded } ! []

        CompleteTask completedTaskId ->
            { model | taskList = Array.filter (\taskId -> taskId /= completedTaskId) model.taskList } ! []

        EditTaskName taskId name ->
            case Dict.get taskId model.tasks of
                Just task ->
                    let
                        updatedTask =
                            { task | name = name }
                    in
                        { model | tasks = Dict.insert taskId updatedTask model.tasks } ! []

                Nothing ->
                    model ! []

        AddNewTask ( assigneeStatus, index ) ->
            let
                localId =
                    toString (Array.length model.taskList)

                task =
                    AsanaTask localId assigneeStatus [] (AsanaWorkspace "" "") "" Nothing

                ( datePicker, datePickerFx ) =
                    DatePicker.init defaultSettings

                taskList =
                    insertTaskAfterIndex ( assigneeStatus, index ) task.id model.taskList
            in
                { model
                    | tasks = Dict.insert task.id task model.tasks
                    , taskList = taskList
                    , datePickers = Dict.insert task.id datePicker model.datePickers
                }
                    ! [ Task.attempt FocusResult (focus (titleInputId task.id)), Cmd.map (ToDatePicker task.id) datePickerFx ]

        ToDatePicker taskId msg ->
            case ( Dict.get taskId model.datePickers, Dict.get taskId model.tasks ) of
                ( Just datePicker, Just task ) ->
                    let
                        ( newDatePicker, datePickerFx, maybeDate ) =
                            DatePicker.update msg datePicker

                        datePickers =
                            Dict.insert taskId newDatePicker model.datePickers

                        updatedTask =
                            case maybeDate of
                                Just date ->
                                    { task | dueOn = Just date }

                                Nothing ->
                                    task

                        tasks =
                            Dict.insert taskId updatedTask model.tasks
                    in
                        { model
                            | datePickers = datePickers
                            , tasks = tasks
                        }
                            ! [ Cmd.map (ToDatePicker taskId) datePickerFx ]

                _ ->
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
