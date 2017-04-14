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


mockProject1 =
    AsanaProject "p1" "Project 1"


mockProject2 =
    AsanaProject "p2" "Project 2"


mockProjects =
    Dict.fromList [ ( mockProject1.id, mockProject1 ), ( mockProject2.id, mockProject2 ) ]


mockTask1 : AsanaTask
mockTask1 =
    AsanaTask "0" (Just mockProject1) "AsanaTask 1" (Just (Date.fromTime 1492178634000))


mockTask2 : AsanaTask
mockTask2 =
    AsanaTask "1" (Just mockProject2) "AsanaTask 2" Nothing


mockTask3 : AsanaTask
mockTask3 =
    AsanaTask "2" Nothing "AsanaTask 3" (Just (Date.fromTime 1488499200000))


mockTasks =
    Dict.fromList [ ( mockTask1.id, mockTask1 ), ( mockTask2.id, mockTask2 ), ( mockTask3.id, mockTask3 ) ]


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
                                { defaultSettings | pickedDate = task.dueDate }

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
        ( datePickers, datePickerFx ) =
            initDatePickers (Dict.values mockTasks)

        initialModel =
            { apiHost = flags.apiHost
            , tasks = mockTasks
            , taskList = Array.fromList [ ( New, mockTask1.id ), ( Today, mockTask2.id ), ( Today, mockTask3.id ) ]
            , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
            , dragDrop = DragDrop.init
            , expanded = { today = True, new = True, upcoming = False, later = False }
            , datePickers = datePickers
            }

        initialCommands =
            datePickerFx
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
                localId =
                    toString (Array.length model.taskList)

                task =
                    AsanaTask localId Nothing "" Nothing

                taskList =
                    insertTaskAfterIndex index task.id model.taskList
            in
                { model | tasks = Dict.insert task.id task model.tasks, taskList = taskList } ! [ Task.attempt FocusResult (focus (titleInputId task.id)) ]

        ToDatePicker taskId msg ->
            case ( Dict.get taskId model.datePickers, Dict.get taskId model.tasks ) of
                ( Just datePicker, Just task ) ->
                    let
                        ( newDatePicker, datePickerFx, maybeDate ) =
                            DatePicker.update msg datePicker

                        datePickers =
                            Dict.insert taskId newDatePicker model.datePickers

                        updatedTask =
                            { task | dueDate = maybeDate }

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
