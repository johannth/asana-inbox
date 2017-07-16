module View exposing (rootView)

import Date exposing (Date)
import Date.Extra.Compare as Compare
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Duration
import Date.Extra.Field as Field exposing (fieldToDateClamp)
import Date.Extra.Format as Format exposing (format, formatUtc, isoMsecOffsetFormat)
import Date.Extra.Period as Period
import Date.Extra.Utils exposing (dayList)
import DatePicker
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput)
import Html5.DragDrop as DragDrop
import Json.Decode as Json
import State exposing (titleInputId)
import Svg
import Svg.Attributes
import Types exposing (..)


expandTasks : Dict String AsanaTask -> Dict String DatePicker.DatePicker -> List String -> List ( AsanaTask, DatePicker.DatePicker )
expandTasks allTasks datePickers taskList =
    List.filterMap
        (\taskId ->
            case ( Dict.get taskId allTasks, Dict.get taskId datePickers ) of
                ( Just task, Just datePicker ) ->
                    Just ( task, datePicker )

                _ ->
                    Nothing
        )
        taskList


rootView : Model -> Html Msg
rootView model =
    let
        hasNoTasks =
            Dict.size model.tasks == 0

        isLoading =
            hasNoTasks && List.length model.accessTokens /= 0
    in
    div [ id "content" ]
        [ h1 [ id "title" ] [ text "Asana Inbox" ]
        , div [ id "body" ]
            [ tokensView model.accessTokenFormExpanded model.accessTokens
            , if hasNoTasks then
                if isLoading then
                    div [] [ text "Loading.." ]
                else
                    div [] []
              else
                tasksView model
            ]
        , div [ id "footer" ]
            [ buildInfoView model.buildInfo
            ]
        ]


tokensView : Bool -> List AsanaAccessToken -> Html Msg
tokensView accessTokenFormExpanded accessTokens =
    let
        expanded =
            accessTokenFormExpanded || List.length accessTokens == 0
    in
    div [ class "accessTokens" ]
        (List.map accessTokenView accessTokens
            ++ [ addNewAccessTokenView expanded ]
        )


addNewAccessTokenView : Bool -> Html Msg
addNewAccessTokenView expanded =
    div [ class "newAccessToken" ]
        (if expanded then
            [ input [ class "newAccessTokenInput", placeholder "Name", onInput AddAccessTokenName ] []
            , input [ class "newAccessTokenInput", placeholder "Access token", onInput AddAccessTokenToken, onEnterPress SaveAccessToken ] []
            ]
         else
            [ toggleAccessTokenButtonView ]
        )


toggleAccessTokenButtonView : Html Msg
toggleAccessTokenButtonView =
    div [ class "expandAccessTokenForm", onClick ToggleAccessTokenForm ]
        [ span []
            [ text "+"
            ]
        ]


accessTokenView : AsanaAccessToken -> Html Msg
accessTokenView accessToken =
    div [ class "accessToken" ]
        [ span [ class "accessTokenName" ] [ text accessToken.name ]
        , div [ class "removeAccessToken", onClick (RemoveAccessToken accessToken) ] [ text "x" ]
        ]


tasksView : Model -> Html Msg
tasksView model =
    div []
        [ div [ class "controls" ]
            [ div [ class "workspaces" ] (List.map (workspaceView model.defaultWorkspace) (Dict.values model.workspaces))
            , togglePlanningModeButton model.inPlanningMode
            ]
        , if model.inPlanningMode then
            planningModeView model
          else
            taskListView model
        ]


togglePlanningModeButton : Bool -> Html Msg
togglePlanningModeButton inPlanningMode =
    div [ class "planningModeToggle" ]
        [ span [] [ text "Planning mode" ]
        , label [ class "switch" ]
            [ input
                ([ type_ "checkbox", onClick TogglePlanningMode ]
                    ++ (if inPlanningMode then
                            [ value "checked" ]
                        else
                            []
                       )
                )
                []
            , span [ class "slider round" ]
                []
            ]
        ]


workspaceView : Maybe String -> AsanaWorkspace -> Html Msg
workspaceView maybeDefaultWorkspace workspace =
    div
        [ onClick (SetDefaultWorkspace workspace.id)
        , class
            ("workspace"
                ++ (if maybeDefaultWorkspace == Just workspace.id then
                        " default"
                    else
                        ""
                   )
            )
        ]
        [ text workspace.name ]


removeTimeFromDate : Date -> Date
removeTimeFromDate date =
    dateFromFields (Date.year date) (Date.month date) (Date.day date) 0 0 0 0


compareIsIgnoringTime : Compare.Compare2 -> Date -> Date -> Bool
compareIsIgnoringTime comparison date1 date2 =
    let
        normalizedDate1 =
            removeTimeFromDate date1

        normalizedDate2 =
            removeTimeFromDate date2
    in
    Compare.is comparison normalizedDate1 normalizedDate2


planningDays : Date -> ( Date, Date, List Date )
planningDays today =
    let
        monday =
            case Date.dayOfWeek today of
                Date.Sun ->
                    Period.add Period.Day 1 today

                _ ->
                    fieldToDateClamp (Field.DayOfWeek ( Date.Mon, Date.Mon )) today

        nextSunday =
            Period.add Period.Day 6 monday
    in
    ( monday, nextSunday, dayList 6 monday ++ [ nextSunday ] )


planningModeActionDialogView : List ( Maybe Date, String, AsanaTaskMutation, List String ) -> Bool -> AsanaTask -> Html Msg
planningModeActionDialogView sectionDays expanded task =
    div [ class "taskAssigneeStatusContainer" ]
        [ div [ class "taskAssigneeStatus", onClick (ToggleAssigneeStatusOverlay task.id) ] []
        , if expanded then
            ul [ class "taskAssigneeStatusOverlay" ]
                (List.map
                    (\( day, title, mutation, _ ) ->
                        li [ class "taskAssigneeStatusOverlayItem", onClick (ApplyMutation task.id mutation) ]
                            [ text title ]
                    )
                    sectionDays
                )
          else
            div [] []
        ]


planningModeView : Model -> Html Msg
planningModeView { today, accessTokenFormExpanded, expandedActionDialog, taskList, tasks, dragDrop, expanded, datePickers } =
    case taskList of
        Just taskList ->
            let
                currentDropTarget =
                    DragDrop.getDropId dragDrop

                ( monday, nextSunday, days ) =
                    planningDays today

                allPossibleTriageTasks =
                    Dict.values tasks
                        |> List.filter (\task -> compareIsIgnoringTime Compare.SameOrBefore (Maybe.withDefault nextSunday task.dueOn) nextSunday)
                        |> List.filter (\task -> not (String.endsWith ":" task.name))

                sectionDays =
                    ([ Nothing ] ++ List.map Just days)
                        |> List.map
                            (\day ->
                                case day of
                                    Just day ->
                                        let
                                            assigneeStatus =
                                                if compareIsIgnoringTime Compare.Same day nextSunday then
                                                    Later
                                                else
                                                    Upcoming

                                            mutation =
                                                { assigneeStatus = assigneeStatus, dueOn = Just (Just day) }
                                        in
                                        ( Just day, format config "%A" day, mutation, List.filter (\task -> Maybe.withDefault False (Maybe.map (compareIsIgnoringTime Compare.Same day) task.dueOn)) allPossibleTriageTasks |> List.map .id )

                                    Nothing ->
                                        ( Nothing, "Triage", { assigneeStatus = Today, dueOn = Just Nothing }, List.filter (\task -> Maybe.withDefault True (Maybe.map (compareIsIgnoringTime Compare.After monday) task.dueOn)) allPossibleTriageTasks |> List.map .id )
                            )

                actionDialogView =
                    \task ->
                        planningModeActionDialogView sectionDays (Just task.id == expandedActionDialog) task

                sections =
                    List.map
                        (\( day, title, mutation, taskIdsForDay ) ->
                            let
                                tasksForSection =
                                    expandTasks tasks datePickers taskIdsForDay
                            in
                            taskListSegmentView { title = title, hideOnEmpty = False } mutation today actionDialogView tasksForSection currentDropTarget True
                        )
                        sectionDays
            in
            div [] sections

        _ ->
            div [] [ text "Loading..." ]


taskListView : Model -> Html Msg
taskListView { today, expandedActionDialog, taskList, tasks, dragDrop, expanded, datePickers } =
    case taskList of
        Just taskList ->
            let
                currentDropTarget =
                    DragDrop.getDropId dragDrop

                newTasks =
                    expandTasks tasks datePickers taskList.new

                todayTasks =
                    expandTasks tasks datePickers taskList.today

                upcomingTasks =
                    expandTasks tasks datePickers taskList.upcoming

                laterTasks =
                    expandTasks tasks datePickers taskList.later

                actionDialogView =
                    \task ->
                        assigneeStatusActionDialogView (Just task.id == expandedActionDialog) task
            in
            div []
                [ taskListSegmentView { title = "New", hideOnEmpty = True } { assigneeStatus = New, dueOn = Nothing } today actionDialogView newTasks currentDropTarget expanded.new
                , taskListSegmentView { title = "Today", hideOnEmpty = False } { assigneeStatus = Today, dueOn = Nothing } today actionDialogView todayTasks currentDropTarget expanded.today
                , taskListSegmentView { title = "Upcoming", hideOnEmpty = False } { assigneeStatus = Upcoming, dueOn = Nothing } today actionDialogView upcomingTasks currentDropTarget expanded.upcoming
                , taskListSegmentView { title = "Later", hideOnEmpty = False } { assigneeStatus = Later, dueOn = Nothing } today actionDialogView laterTasks currentDropTarget expanded.later
                ]

        Nothing ->
            div [] [ text "Loading..." ]


filterTasksByStatus : AssigneeStatus -> List ( Int, AssigneeStatus, AsanaTask, DatePicker.DatePicker ) -> List ( Int, AsanaTask, DatePicker.DatePicker )
filterTasksByStatus assigneeStatus tasks =
    List.filter (\( _, taskAssigneeStatus, _, _ ) -> taskAssigneeStatus == assigneeStatus) tasks
        |> List.map (\( index, _, task, datePicker ) -> ( index, task, datePicker ))


type alias TaskListSegmentConfig =
    { title : String
    , hideOnEmpty : Bool
    }


taskListSegmentView : TaskListSegmentConfig -> AsanaTaskMutation -> Date -> (AsanaTask -> Html Msg) -> List ( AsanaTask, DatePicker.DatePicker ) -> Maybe DropTarget -> Bool -> Html Msg
taskListSegmentView config mutation today actionDialogView tasks currentDropTarget expanded =
    let
        taskViews =
            if expanded then
                List.filter (\( task, _ ) -> not task.completed) tasks
                    |> List.map
                        (\( task, datePicker ) ->
                            taskView today actionDialogView datePicker currentDropTarget mutation task
                        )
            else
                []

        dropView =
            fakeDropView currentDropTarget (End mutation)
    in
    if config.hideOnEmpty && List.length tasks == 0 then
        text ""
    else
        div []
            [ taskListHeaderView mutation.assigneeStatus config.title expanded
            , ul [ class "tasks" ]
                (taskViews ++ [ dropView ])
            ]


taskListHeaderView : AssigneeStatus -> String -> Bool -> Html Msg
taskListHeaderView assigneeStatus title expanded =
    h2
        [ class "tasksHeader", onClick (ToggleExpanded assigneeStatus) ]
        [ div
            [ class
                ("tasksHeaderTriangleIcon"
                    ++ (if not expanded then
                            " closed"
                        else
                            ""
                       )
                )
            ]
            [ triangle ]
        , text title
        ]


triangle : Html Msg
triangle =
    Svg.svg [ Svg.Attributes.viewBox "0 0 32 32" ]
        [ Svg.path [ Svg.Attributes.d "M7.207,13.707L16.5,23l9.293-9.293c0.63-0.63,0.184-1.707-0.707-1.707H7.914C7.023,12,6.577,13.077,7.207,13.707z" ] []
        ]


classNameIfOnTop : Maybe DropTarget -> DropTarget -> String
classNameIfOnTop maybeCurrentDropTarget dropTarget =
    case maybeCurrentDropTarget of
        Just currentDropTarget ->
            if equalDropTarget currentDropTarget dropTarget then
                "onTop"
            else
                ""

        Nothing ->
            ""


taskView : Date -> (AsanaTask -> Html Msg) -> DatePicker.DatePicker -> Maybe DropTarget -> AsanaTaskMutation -> AsanaTask -> Html Msg
taskView today actionDialogView datePicker currentDropTarget mutation task =
    let
        dropTarget =
            Before mutation task.id

        classNames =
            "task " ++ classNameIfOnTop currentDropTarget dropTarget

        isHeading =
            String.endsWith ":" task.name
    in
    li
        ([ class classNames ] ++ DragDrop.draggable DragDropMsg (DragTarget task.assigneeStatus task.id) ++ DragDrop.droppable DragDropMsg dropTarget)
        (if isHeading then
            [ div [ class "taskDragHandle" ] [ dragHandle ]
            , div [ class "taskTitleAsHeader" ] [ h3 [] [ taskTitleView task.id task.assigneeStatus task.name ] ]
            ]
         else
            [ div [ class "taskDragHandle" ] [ dragHandle ]
            , taskCompletionButton task.id
            , div [ class "taskWorkspaceAndTitle" ]
                [ div [ class "taskWorkspace" ]
                    [ text task.workspace.name ]
                , taskTitleView task.id task.assigneeStatus task.name
                ]
            ]
                ++ (if task.url /= "" then
                        [ a [ class "taskUrl", target "_blank", href task.url ] [ text "Link" ] ]
                    else
                        []
                   )
                ++ [ taskDatePickerView today datePicker task.id task.dueOn
                   , actionDialogView task
                   ]
        )


dragHandle : Html Msg
dragHandle =
    Svg.svg [ Svg.Attributes.viewBox "0 0 32 32" ]
        [ Svg.path [ Svg.Attributes.d "M 14 5.5 a 3 3 0 1 1 -3 -3 A 3 3 0 0 1 14 5.5 Z m 7 3 a 3 3 0 1 0 -3 -3 A 3 3 0 0 0 21 8.5 Z m -10 4 a 3 3 0 1 0 3 3 A 3 3 0 0 0 11 12.5 Z m 10 0 a 3 3 0 1 0 3 3 A 3 3 0 0 0 21 12.5 Z m -10 10 a 3 3 0 1 0 3 3 A 3 3 0 0 0 11 22.5 Z m 10 0 a 3 3 0 1 0 3 3 A 3 3 0 0 0 21 22.5 Z" ] []
        ]


taskCompletionButton : String -> Html Msg
taskCompletionButton taskId =
    div [ class "taskCompletionButton", onClick (CompleteTask taskId) ]
        [ checkMark
        ]


checkMark : Html Msg
checkMark =
    Svg.svg [ Svg.Attributes.viewBox "0 0 32 32" ]
        [ Svg.polygon [ Svg.Attributes.points "27.672,4.786 10.901,21.557 4.328,14.984 1.5,17.812 10.901,27.214 30.5,7.615 " ] []
        ]


taskTitleView : String -> AssigneeStatus -> String -> Html Msg
taskTitleView taskId assigneeStatus title =
    input [ class "taskTitle", id (titleInputId taskId), onInput (EditTaskName taskId), onBlur (StopEditTaskName taskId), onEnterPress (AddNewTask assigneeStatus taskId), value title ] []


removeTimePart : Date -> Date
removeTimePart date =
    Date.Extra.Create.dateFromFields (Date.year date) (Date.month date) (Date.day date) 0 0 0 0


friendlyDate : Date -> Date -> ( String, String )
friendlyDate today date =
    let
        todayDay =
            removeTimePart today

        dateDay =
            removeTimePart date

        sevenDays =
            Date.Extra.Duration.add Date.Extra.Duration.Week 1 todayDay

        tomorrow =
            Date.Extra.Duration.add Date.Extra.Duration.Day 1 todayDay
    in
    if Compare.is Compare.Before dateDay todayDay then
        ( "overdue", "Overdue" )
    else if Compare.is Compare.Same dateDay todayDay then
        ( "today", "Today" )
    else if Compare.is Compare.Same dateDay tomorrow then
        ( "tomorrow", "Tomorrow" )
    else if Compare.is Compare.SameOrBefore dateDay sevenDays then
        ( "", format config "%A" date )
    else if Date.year todayDay == Date.year dateDay then
        ( "", format config "%e %B" date )
    else
        ( "", format config "%e %B, %Y" date )


taskDatePickerView : Date -> DatePicker.DatePicker -> String -> Maybe Date -> Html Msg
taskDatePickerView today datePicker taskId maybeDueDate =
    div [ class "datePickerContainer" ]
        [ case maybeDueDate of
            Just dueDate ->
                let
                    ( className, formattedDate ) =
                        friendlyDate today dueDate
                in
                div [ class ("datePicker" ++ " " ++ className) ] [ text formattedDate ]

            Nothing ->
                text ""
        , DatePicker.view datePicker
            |> Html.map (ToDatePicker taskId)
        ]


onEnterPress : Msg -> Attribute Msg
onEnterPress tagger =
    Html.Events.on "keydown"
        (Json.map
            (\keyCode ->
                if keyCode == 13 then
                    tagger
                else
                    Void
            )
            Html.Events.keyCode
        )


assigneeStatusActionDialogView : Bool -> AsanaTask -> Html Msg
assigneeStatusActionDialogView expanded task =
    div [ class "taskAssigneeStatusContainer" ]
        [ div [ class "taskAssigneeStatus", onClick (ToggleAssigneeStatusOverlay task.id) ] []
        , if expanded then
            ul [ class "taskAssigneeStatusOverlay" ]
                [ li [ class "taskAssigneeStatusOverlayItem", onClick (ApplyMutation task.id { assigneeStatus = Today, dueOn = Nothing }) ]
                    [ text "Mark for Today" ]
                , li [ class "taskAssigneeStatusOverlayItem", onClick (ApplyMutation task.id { assigneeStatus = Upcoming, dueOn = Nothing }) ]
                    [ text "Mark for Upcoming" ]
                , li [ class "taskAssigneeStatusOverlayItem", onClick (ApplyMutation task.id { assigneeStatus = Later, dueOn = Nothing }) ]
                    [ text "Mark for Later" ]
                ]
          else
            div [] []
        ]


fakeDropView : Maybe DropTarget -> DropTarget -> Html Msg
fakeDropView currentDropTarget dropTarget =
    let
        classNames =
            "fakeDropView " ++ classNameIfOnTop currentDropTarget dropTarget
    in
    li
        ([ class classNames ] ++ DragDrop.droppable DragDropMsg dropTarget)
        []


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ String.slice 0 8 buildInfo.version ++ "-" ++ buildInfo.tier)
