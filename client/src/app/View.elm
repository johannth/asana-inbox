module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Types exposing (..)
import Html5.DragDrop as DragDrop
import Array exposing (Array)


expandTasks : Dict String Task -> List String -> List Task
expandTasks allTasks taskIds =
    List.filterMap (\taskId -> Dict.get taskId allTasks) taskIds


rootView : Model -> Html Msg
rootView { taskList, tasks, dragDrop, buildInfo, expanded } =
    let
        taskCategories =
            Array.map (\( category, _ ) -> category) taskList

        taskIds =
            Array.map (\( _, taskId ) -> taskId) taskList

        expandedTasks =
            expandTasks tasks (Array.toList taskIds)

        allTasksWithIndexAndCategory =
            List.map3 (\index category task -> ( index, category, task )) (List.range 0 (List.length expandedTasks)) (Array.toList taskCategories) expandedTasks

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


taskListView : Bool -> TaskCategory -> String -> List ( Int, TaskCategory, Task ) -> Maybe DragDropIndex -> Bool -> Html Msg
taskListView hideOnEmpty category title allTasks maybeDropId expanded =
    let
        tasksInThisCategory =
            List.filter (\( _, taskCategory, t ) -> taskCategory == category) allTasks

        taskViews =
            (List.map (\( index, _, task ) -> taskView maybeDropId category index task) tasksInThisCategory)

        dropView =
            fakeDropView maybeDropId category ((List.length tasksInThisCategory) + 1)
    in
        if hideOnEmpty && List.length tasksInThisCategory == 0 then
            text ""
        else
            div []
                [ h2 [ class "tasksHeader", onClick (ToggleExpanded category) ] [ text title ]
                , ul [ class "tasks" ]
                    (if expanded then
                        taskViews ++ [ dropView ]
                     else
                        [ dropView ]
                    )
                ]


classNameIfOnTop : Maybe DragDropIndex -> TaskCategory -> Int -> String
classNameIfOnTop maybeDropId category index =
    case maybeDropId of
        Just ( dropCategory, dropIndex ) ->
            if dropCategory == category && dropIndex == index then
                " onTop"
            else
                ""

        Nothing ->
            ""


taskView : Maybe DragDropIndex -> TaskCategory -> Int -> Task -> Html Msg
taskView maybeDropId category index task =
    let
        classNames =
            "task" ++ (classNameIfOnTop maybeDropId category index)
    in
        li
            ([ class classNames ] ++ DragDrop.draggable DragDropMsg ( category, index ) ++ DragDrop.droppable DragDropMsg ( category, index ))
            [ text task.title
            ]


fakeDropView : Maybe DragDropIndex -> TaskCategory -> Int -> Html Msg
fakeDropView maybeDropId category index =
    let
        classNames =
            "fakeDropView" ++ (classNameIfOnTop maybeDropId category index)
    in
        li
            ([ class classNames ] ++ DragDrop.droppable DragDropMsg ( category, index ))
            []


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ (String.slice 0 8 buildInfo.version) ++ "-" ++ buildInfo.tier)
