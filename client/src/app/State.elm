module State exposing (..)

import Types exposing (..)
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)
import Dict exposing (Dict)
import Dom exposing (focus)
import Task
import DatePicker exposing (defaultSettings)
import Api
import LocalStorage


accessTokensStorageKey : String
accessTokensStorageKey =
    "accessTokens"


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
            , accessTokenFormExpanded = False
            , newAccessTokenName = ""
            , newAccessTokenToken = ""
            , accessTokens = []
            , tasks = Dict.empty
            , taskList = Array.empty
            , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
            , dragDrop = DragDrop.init
            , expanded = { today = True, new = True, upcoming = False, later = False }
            , datePickers = Dict.empty
            }

        initialCommands =
            [ LocalStorage.getItem accessTokensStorageKey ]
    in
        initialModel ! initialCommands


moveItemInArray : Int -> Int -> Array comparable -> Array comparable
moveItemInArray fromIndex toIndex array =
    case Array.get fromIndex array of
        Just item ->
            let
                filterItem =
                    Array.filter (\x -> x /= item)

                firstHalf =
                    (Array.slice 0 toIndex array) |> filterItem |> Array.toList

                secondHalf =
                    (Array.slice toIndex (Array.length array)) array |> filterItem |> Array.toList
            in
                Array.fromList (firstHalf ++ [ item ] ++ secondHalf)

        Nothing ->
            array


insertAfterIndex : Int -> a -> Array a -> Array a
insertAfterIndex index item array =
    let
        firstHalf =
            (Array.slice 0 (index + 1) array) |> Array.toList

        secondHalf =
            (Array.slice (index + 1) (Array.length array) array) |> Array.toList
    in
        Array.fromList (firstHalf ++ [ item ] ++ secondHalf)


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
    LocalStorage.setItem (LocalStorage.encodeToItem accessTokensStorageKey (Api.encodeAccessTokens accessTokens))


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
            let
                maybeAccessTokens =
                    case item.key of
                        accessTokensStorageKey ->
                            LocalStorage.decodeToType item Api.decodeAccessTokens
            in
                case maybeAccessTokens of
                    Just accessTokens ->
                        { model | accessTokens = accessTokens } ! [ Api.getTasks model.apiHost accessTokens ]

                    Nothing ->
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
            case Dict.get completedTaskId model.tasks of
                Just task ->
                    { model | taskList = Array.filter (\taskId -> taskId /= completedTaskId) model.taskList } ! [ Api.updateTask model.apiHost model.accessTokens task Complete ]

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

        AddNewTask ( assigneeStatus, _, index ) ->
            let
                localId =
                    toString (Array.length model.taskList)

                task =
                    AsanaTask localId assigneeStatus [] (AsanaWorkspace "" "") "" Nothing

                ( datePicker, datePickerFx ) =
                    DatePicker.init defaultSettings

                taskList =
                    insertAfterIndex index task.id model.taskList
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

        TaskUpdated result ->
            model ! []

        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop

                ( taskList, tasks, commands ) =
                    case result of
                        Just ( ( dragAssigneeStatus, dragTaskId, dragIndex ), ( dropAssigneeStatus, _, dropIndex ) ) ->
                            case Dict.get dragTaskId model.tasks of
                                Just task ->
                                    ( moveItemInArray dragIndex dropIndex model.taskList
                                    , updateAssigneeStatus dragTaskId dropAssigneeStatus model.tasks
                                    , [ Api.updateTask model.apiHost model.accessTokens task (UpdateAssigneeStatus dropAssigneeStatus) ]
                                    )

                                Nothing ->
                                    ( model.taskList, model.tasks, [] )

                        Nothing ->
                            ( model.taskList, model.tasks, [] )
            in
                { model
                    | dragDrop = model_
                    , taskList = taskList
                    , tasks = tasks
                }
                    ! commands
