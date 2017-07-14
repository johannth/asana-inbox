module State exposing (..)

import Api
import Date
import DatePicker exposing (defaultSettings)
import Dict exposing (Dict)
import Dom exposing (focus)
import Html5.DragDrop as DragDrop
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage
import Navigation
import Task
import Types exposing (..)


accessTokensStorageKey : String
accessTokensStorageKey =
    "accessTokens"


defaultWorkspaceStorageKey : String
defaultWorkspaceStorageKey =
    "defaultWorkspace"


taskListStorageKey : String
taskListStorageKey =
    "taskList"


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
            , today = Date.fromTime flags.today
            , inPlanningMode = False
            , accessTokenFormExpanded = False
            , newAccessTokenName = ""
            , newAccessTokenToken = ""
            , accessTokens = []
            , tasks = Dict.empty
            , taskList = Nothing
            , workspaces = Dict.empty
            , defaultWorkspace = Nothing
            , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
            , dragDrop = DragDrop.init
            , expanded = { today = True, new = True, upcoming = False, later = False }
            , datePickers = Dict.empty
            , expandedAssigneeStatusOverlay = Nothing
            }

        initialCommands =
            [ LocalStorage.getItem accessTokensStorageKey, LocalStorage.getItem defaultWorkspaceStorageKey, LocalStorage.getItem taskListStorageKey ]
    in
    initialModel ! initialCommands


toggleExpandedState : AssigneeStatus -> ExpandedState -> ExpandedState
toggleExpandedState assigneeStatus currentState =
    case assigneeStatus of
        Today ->
            { currentState | today = not currentState.today }

        New ->
            { currentState | new = not currentState.new }

        Upcoming ->
            { currentState | upcoming = not currentState.upcoming }

        Later ->
            { currentState | later = not currentState.later }


updateAssigneeStatus : String -> AssigneeStatus -> Dict String AsanaTask -> Dict String AsanaTask
updateAssigneeStatus taskId assigneeStatus tasks =
    case Dict.get taskId tasks of
        Just task ->
            let
                updatedTask =
                    { task | assigneeStatus = assigneeStatus }
            in
            Dict.insert taskId updatedTask tasks

        Nothing ->
            tasks


saveApiTokens : List AsanaAccessToken -> Cmd Msg
saveApiTokens accessTokens =
    LocalStorage.setItem accessTokensStorageKey accessTokens Api.encodeAccessTokens


moveTaskInTaskList : DragTarget -> DropTarget -> Maybe TaskList -> Maybe TaskList
moveTaskInTaskList dragTarget dropTarget maybeTaskList =
    case maybeTaskList of
        Just taskList ->
            Just (moveTaskInTaskList_ dragTarget dropTarget taskList)

        Nothing ->
            Nothing


moveTaskInTaskList_ : DragTarget -> DropTarget -> TaskList -> TaskList
moveTaskInTaskList_ dragTarget dropTarget taskList =
    let
        ( dropAssigneeStatus, maybeDropTaskId, inserter ) =
            case dropTarget of
                Before assigneeStatus dropTaskId ->
                    ( assigneeStatus
                    , Just dropTaskId
                    , \taskIdToInsert taskIds ->
                        case taskIds of
                            [] ->
                                [ taskIdToInsert ]

                            _ ->
                                List.concatMap
                                    (\taskId ->
                                        if taskId == dropTaskId then
                                            [ taskIdToInsert, taskId ]
                                        else
                                            [ taskId ]
                                    )
                                    taskIds
                    )

                After assigneeStatus dropTaskId ->
                    ( assigneeStatus
                    , Just dropTaskId
                    , \taskIdToInsert taskIds ->
                        case taskIds of
                            [] ->
                                [ taskIdToInsert ]

                            _ ->
                                List.concatMap
                                    (\taskId ->
                                        if taskId == dropTaskId then
                                            [ taskId, taskIdToInsert ]
                                        else
                                            [ taskId ]
                                    )
                                    taskIds
                    )

                End assigneeStatus ->
                    ( assigneeStatus, Nothing, \taskIdToInsert taskIds -> taskIds ++ [ taskIdToInsert ] )
    in
    if Just dragTarget.targetId == maybeDropTaskId then
        taskList
    else
        removeIdFromTaskList dragTarget.assigneeStatus dragTarget.targetId taskList
            |> insertIdIntoTaskList dropAssigneeStatus dragTarget.targetId inserter


removeIdFromTaskList : AssigneeStatus -> String -> TaskList -> TaskList
removeIdFromTaskList assigneeStatus taskIdToRemove taskList =
    updateTaskList (List.filter (\taskId -> taskId /= taskIdToRemove)) assigneeStatus taskList


insertIdIntoTaskList : AssigneeStatus -> String -> (String -> List String -> List String) -> TaskList -> TaskList
insertIdIntoTaskList assigneeStatus taskId inserter taskList =
    updateTaskList (inserter taskId) assigneeStatus taskList


updateTaskList : (List String -> List String) -> AssigneeStatus -> TaskList -> TaskList
updateTaskList updater assigneeStatus taskList =
    case assigneeStatus of
        New ->
            { taskList | new = updater taskList.new }

        Today ->
            { taskList | today = updater taskList.today }

        Upcoming ->
            { taskList | upcoming = updater taskList.upcoming }

        Later ->
            { taskList | later = updater taskList.later }


saveTaskList : Maybe TaskList -> Cmd Msg
saveTaskList maybeTaskList =
    case maybeTaskList of
        Just taskList ->
            LocalStorage.setItem taskListStorageKey taskList Api.encodeTaskList

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Void ->
            model ! []

        FocusResult _ ->
            model ! []

        UrlChange newLocation ->
            model ! []

        ReceiveItem item ->
            if item.key == accessTokensStorageKey then
                let
                    maybeAccessTokens =
                        LocalStorage.decodeToType item Api.decodeAccessTokens
                in
                case maybeAccessTokens of
                    Just accessTokens ->
                        { model | accessTokens = accessTokens } ! [ Api.getTasks model.apiHost accessTokens ]

                    Nothing ->
                        model ! []
            else if item.key == defaultWorkspaceStorageKey then
                let
                    maybeDefaultWorkspace =
                        LocalStorage.decodeToType item Decode.string
                in
                case maybeDefaultWorkspace of
                    Just accessTokens ->
                        { model | defaultWorkspace = maybeDefaultWorkspace } ! []

                    Nothing ->
                        model ! []
            else if item.key == taskListStorageKey then
                let
                    taskList =
                        LocalStorage.decodeToType item Api.decodeTaskList
                in
                { model | taskList = taskList } ! []
            else
                model ! []

        ToggleAccessTokenForm ->
            { model | accessTokenFormExpanded = not model.accessTokenFormExpanded } ! []

        AddAccessTokenName name ->
            { model | newAccessTokenName = name } ! []

        AddAccessTokenToken token ->
            { model | newAccessTokenToken = token } ! []

        SaveAccessToken ->
            let
                newAccessToken =
                    AsanaAccessToken model.newAccessTokenName model.newAccessTokenToken

                updatedModel =
                    { model | accessTokens = model.accessTokens ++ [ newAccessToken ], newAccessTokenName = "", newAccessTokenToken = "", accessTokenFormExpanded = False }
            in
            updatedModel ! [ Api.getTasks updatedModel.apiHost updatedModel.accessTokens, saveApiTokens updatedModel.accessTokens ]

        RemoveAccessToken accessToken ->
            let
                updatedModel =
                    { model | accessTokens = List.filter (\token -> token /= accessToken) model.accessTokens }
            in
            updatedModel ! [ saveApiTokens updatedModel.accessTokens ]

        LoadTasks (Err message) ->
            let
                x =
                    Debug.log "Error" message
            in
            model ! []

        LoadTasks (Ok newTasks) ->
            let
                tasks =
                    Dict.fromList (List.map (\task -> ( task.id, task )) newTasks)

                new =
                    Dict.values tasks
                        |> List.filter (\task -> task.assigneeStatus == New)
                        |> List.map .id

                currentTodayTasks =
                    Maybe.withDefault [] (Maybe.map .today model.taskList)

                currentTodayTasksLookUp =
                    Dict.fromList (List.map2 (,) currentTodayTasks (List.range 0 (List.length currentTodayTasks)))

                today =
                    Dict.values tasks
                        |> List.filter (\task -> task.assigneeStatus == Today)
                        |> List.sortBy (.id >> flip Dict.get currentTodayTasksLookUp >> Maybe.withDefault -1)
                        |> List.map .id

                sortByDate =
                    List.sortBy (.dueOn >> Maybe.map Date.toTime >> Maybe.withDefault 0)

                upcoming =
                    Dict.values tasks
                        |> List.filter (\task -> task.assigneeStatus == Upcoming)
                        |> sortByDate
                        |> List.map .id

                later =
                    Dict.values tasks
                        |> List.filter (\task -> task.assigneeStatus == Later)
                        |> sortByDate
                        |> List.map .id

                taskList =
                    TaskList new today upcoming later

                workspaces =
                    Dict.fromList (List.map (\task -> ( task.workspace.id, task.workspace )) (Dict.values tasks))

                ( datePickers, datePickerFx ) =
                    initDatePickers (Dict.values tasks)
            in
            { model
                | taskList = Just taskList
                , tasks = tasks
                , datePickers = datePickers
                , workspaces = workspaces
            }
                ! datePickerFx

        SetDefaultWorkspace workspaceId ->
            { model | defaultWorkspace = Just workspaceId } ! [ LocalStorage.setItem defaultWorkspaceStorageKey workspaceId Encode.string ]

        ToggleExpanded taskCategory ->
            { model | expanded = toggleExpandedState taskCategory model.expanded } ! []

        CompleteTask completedTaskId ->
            case Dict.get completedTaskId model.tasks of
                Just task ->
                    let
                        updatedTask =
                            { task | completed = True }
                    in
                    { model | tasks = Dict.insert updatedTask.id updatedTask model.tasks } ! [ Api.updateTask model.apiHost model.accessTokens task Complete ]

                Nothing ->
                    model ! []

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

        StopEditTaskName taskId ->
            case Dict.get taskId model.tasks of
                Just task ->
                    let
                        command =
                            if String.startsWith "local-" task.id then
                                Api.createTask model.apiHost model.accessTokens task
                            else
                                Api.updateTask model.apiHost model.accessTokens task (UpdateName task.name)
                    in
                    model ! [ command ]

                Nothing ->
                    model ! []

        AddNewTask assigneeStatus taskId ->
            case Maybe.andThen (\workspaceId -> Dict.get workspaceId model.workspaces) model.defaultWorkspace of
                Just workspace ->
                    let
                        localId =
                            "local-" ++ toString (Dict.size model.tasks)

                        task =
                            AsanaTask localId "" assigneeStatus [] workspace "" Nothing False

                        ( datePicker, datePickerFx ) =
                            DatePicker.init defaultSettings

                        taskList =
                            moveTaskInTaskList (DragTarget task.assigneeStatus task.id) (After assigneeStatus taskId) model.taskList
                    in
                    { model
                        | tasks = Dict.insert task.id task model.tasks
                        , taskList = taskList
                        , datePickers = Dict.insert task.id datePicker model.datePickers
                    }
                        ! [ Task.attempt FocusResult (focus (titleInputId task.id)), Cmd.map (ToDatePicker task.id) datePickerFx ]

                Nothing ->
                    model ! []

        ToDatePicker taskId msg ->
            case ( Dict.get taskId model.datePickers, Dict.get taskId model.tasks ) of
                ( Just datePicker, Just task ) ->
                    let
                        ( newDatePicker, datePickerFx, maybeDate ) =
                            DatePicker.update msg datePicker

                        datePickers =
                            Dict.insert taskId newDatePicker model.datePickers

                        ( updatedTask, command ) =
                            case maybeDate of
                                Just date ->
                                    ( { task | dueOn = Just date }, Api.updateTask model.apiHost model.accessTokens task (UpdateDueOn date) )

                                Nothing ->
                                    ( task, Cmd.none )

                        tasks =
                            Dict.insert taskId updatedTask model.tasks
                    in
                    { model
                        | datePickers = datePickers
                        , tasks = tasks
                    }
                        ! [ Cmd.map (ToDatePicker taskId) datePickerFx, command ]

                _ ->
                    model ! []

        TaskCreated localTaskId (Err message) ->
            model ! []

        TaskCreated localTaskId (Ok task) ->
            let
                tasks =
                    Dict.insert task.id task model.tasks

                taskList =
                    Maybe.map
                        (updateTaskList
                            (List.map
                                (\taskId ->
                                    if taskId == localTaskId then
                                        task.id
                                    else
                                        taskId
                                )
                            )
                            task.assigneeStatus
                        )
                        model.taskList

                datePickers =
                    case Dict.get localTaskId model.datePickers of
                        Just datePicker ->
                            Dict.remove localTaskId model.datePickers |> Dict.insert task.id datePicker

                        Nothing ->
                            model.datePickers

                updatedModel =
                    { model
                        | tasks = tasks
                        , taskList = taskList
                        , datePickers = datePickers
                    }
            in
            updatedModel ! [ saveTaskList updatedModel.taskList ]

        TaskUpdated result ->
            model ! []

        DragDropMsg dragDropMessage ->
            let
                ( dragDrop, result ) =
                    DragDrop.update dragDropMessage model.dragDrop

                ( taskList, tasks, commands ) =
                    case result of
                        Just ( dragTarget, dropTarget ) ->
                            let
                                newAssigneeStatus =
                                    assigneeStatusFromDropTarget dropTarget
                            in
                            case Dict.get dragTarget.targetId model.tasks of
                                Just task ->
                                    ( moveTaskInTaskList dragTarget dropTarget model.taskList
                                    , updateAssigneeStatus dragTarget.targetId newAssigneeStatus model.tasks
                                    , [ Api.updateTask model.apiHost model.accessTokens task (UpdateAssigneeStatus newAssigneeStatus)
                                      ]
                                    )

                                Nothing ->
                                    ( model.taskList, model.tasks, [] )

                        Nothing ->
                            ( model.taskList, model.tasks, [] )

                updatedModel =
                    { model
                        | dragDrop = dragDrop
                        , taskList = taskList
                        , tasks = tasks
                    }
            in
            updatedModel ! (commands ++ [ saveTaskList updatedModel.taskList ])

        ToggleAssigneeStatusOverlay taskId ->
            { model
                | expandedAssigneeStatusOverlay =
                    if model.expandedAssigneeStatusOverlay == Just taskId then
                        Nothing
                    else
                        Just taskId
            }
                ! []

        SetAssigneeStatus taskId assigneeStatus ->
            case Dict.get taskId model.tasks of
                Just task ->
                    let
                        updatedTask =
                            { task | assigneeStatus = assigneeStatus }

                        taskList =
                            moveTaskInTaskList (DragTarget task.assigneeStatus task.id) (End assigneeStatus) model.taskList

                        updatedModel =
                            { model
                                | tasks = Dict.insert taskId updatedTask model.tasks
                                , taskList = taskList
                                , expandedAssigneeStatusOverlay = Nothing
                            }
                    in
                    updatedModel
                        ! [ Api.updateTask model.apiHost model.accessTokens task (UpdateAssigneeStatus assigneeStatus), saveTaskList updatedModel.taskList ]

                Nothing ->
                    model ! []

        TogglePlanningMode ->
            { model | inPlanningMode = not model.inPlanningMode } ! []
