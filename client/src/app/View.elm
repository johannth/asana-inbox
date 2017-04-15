module View exposing (rootView)

import Html exposing (..)
import Svg
import Svg.Attributes
import Date exposing (Date)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format, formatUtc, isoMsecOffsetFormat)
import DatePicker
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Types exposing (..)
import Html5.DragDrop as DragDrop
import Array exposing (Array)
import Json.Decode as Json
import State exposing (titleInputId)


expandTasks : Dict String AsanaTask -> Dict String DatePicker.DatePicker -> List String -> List ( AsanaTask, DatePicker.DatePicker )
expandTasks allTasks datePickers taskIds =
    List.filterMap
        (\taskId ->
            let
                task =
                    Dict.get taskId allTasks

                datePicker =
                    Dict.get taskId datePickers
            in
                case ( task, datePicker ) of
                    ( Just task, Just datePicker ) ->
                        Just ( task, datePicker )

                    _ ->
                        Nothing
        )
        taskIds


rootView : Model -> Html Msg
rootView { taskList, tasks, dragDrop, buildInfo, expanded, datePickers } =
    let
        taskCategories =
            Array.map (\( category, _ ) -> category) taskList

        taskIds =
            Array.map (\( _, taskId ) -> taskId) taskList

        expandedTasks =
            expandTasks tasks datePickers (Array.toList taskIds)

        allTasksWithIndexAndCategory =
            List.map3 (\index category ( task, datePicker ) -> ( index, category, task, datePicker )) (List.range 0 (List.length expandedTasks)) (Array.toList taskCategories) expandedTasks

        dropId =
            DragDrop.getDropId dragDrop
    in
        div [ id "content" ]
            [ h1 [ id "title" ] [ text "Asana Inbox" ]
            , div [ id "body" ]
                [ taskListView True New "New" allTasksWithIndexAndCategory dropId expanded.new
                , taskListView False Today "Today" allTasksWithIndexAndCategory dropId expanded.today
                , taskListView False Upcoming "Upcoming" allTasksWithIndexAndCategory dropId expanded.upcoming
                , taskListView False Later "Later" allTasksWithIndexAndCategory dropId expanded.later
                ]
            , div [ id "footer" ]
                [ buildInfoView buildInfo
                ]
            ]


taskListView : Bool -> AsanaTaskCategory -> String -> List ( Int, AsanaTaskCategory, AsanaTask, DatePicker.DatePicker ) -> Maybe TaskListIndex -> Bool -> Html Msg
taskListView hideOnEmpty category title allTasks maybeDropId expanded =
    let
        tasksInThisCategory =
            List.filter (\( _, taskCategory, _, _ ) -> taskCategory == category) allTasks

        taskViews =
            (List.map (\( index, _, task, datePicker ) -> taskView datePicker maybeDropId ( category, index ) task) tasksInThisCategory)

        dropView =
            fakeDropView maybeDropId ( category, ((List.length tasksInThisCategory) + 1) )
    in
        if hideOnEmpty && List.length tasksInThisCategory == 0 then
            text ""
        else
            div []
                [ h2
                    [ class "tasksHeader", onClick (ToggleExpanded category) ]
                    [ div
                        [ class
                            ("tasksHeaderTriangleIcon"
                                ++ if not expanded then
                                    " closed"
                                   else
                                    ""
                            )
                        ]
                        [ triangle ]
                    , text title
                    ]
                , ul [ class "tasks" ]
                    (if expanded then
                        taskViews ++ [ dropView ]
                     else
                        [ dropView ]
                    )
                ]


triangle : Html Msg
triangle =
    Svg.svg [ Svg.Attributes.viewBox "0 0 32 32" ]
        [ Svg.path [ Svg.Attributes.d "M7.207,13.707L16.5,23l9.293-9.293c0.63-0.63,0.184-1.707-0.707-1.707H7.914C7.023,12,6.577,13.077,7.207,13.707z" ] []
        ]


classNameIfOnTop : Maybe TaskListIndex -> TaskListIndex -> String
classNameIfOnTop maybeDropId ( category, index ) =
    case maybeDropId of
        Just ( dropCategory, dropIndex ) ->
            if dropCategory == category && dropIndex == index then
                " onTop"
            else
                ""

        Nothing ->
            ""


taskView : DatePicker.DatePicker -> Maybe TaskListIndex -> TaskListIndex -> AsanaTask -> Html Msg
taskView datePicker maybeDropId index task =
    let
        classNames =
            "task" ++ (classNameIfOnTop maybeDropId index)
    in
        li
            ([ class classNames ] ++ DragDrop.draggable DragDropMsg index ++ DragDrop.droppable DragDropMsg index)
            [ div [ class "taskDragHandle" ] [ dragHandle ]
            , taskCompletionButton task.id
            , taskTitleView index task.id task.title
            , taskDatePickerView datePicker task.id task.dueDate
            ]


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


taskTitleView : TaskListIndex -> String -> String -> Html Msg
taskTitleView index taskId title =
    input [ class "taskTitle", id (titleInputId taskId), onInput (EditTaskTitle taskId), onEnterPress (AddNewTask index), value title ] []


taskDatePickerView : DatePicker.DatePicker -> String -> Maybe Date -> Html Msg
taskDatePickerView datePicker taskId maybeDueDate =
    div [ class "datePickerContainer" ]
        [ (case maybeDueDate of
            Just dueDate ->
                let
                    formattedDate =
                        format config config.format.date dueDate
                in
                    div [ class "datePicker" ] [ text formattedDate ]

            Nothing ->
                text ""
          )
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


fakeDropView : Maybe TaskListIndex -> TaskListIndex -> Html Msg
fakeDropView maybeDropId index =
    let
        classNames =
            "fakeDropView" ++ (classNameIfOnTop maybeDropId index)
    in
        li
            ([ class classNames ] ++ DragDrop.droppable DragDropMsg index)
            []


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ (String.slice 0 8 buildInfo.version) ++ "-" ++ buildInfo.tier)
