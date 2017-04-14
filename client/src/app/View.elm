module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Types exposing (..)
import Html5.DragDrop as DragDrop
import Array exposing (Array)


expandTasks : Dict String Task -> List String -> List Task
expandTasks allTasks taskIds =
    List.filterMap (\taskId -> Dict.get taskId allTasks) taskIds


rootView : Model -> Html Msg
rootView { taskList, tasks, dragDrop, buildInfo } =
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
                [ taskListView dropId New "New" allTasksWithIndexAndCategory
                , taskListView dropId Today "Today" allTasksWithIndexAndCategory
                , taskListView dropId Upcoming "Upcoming" allTasksWithIndexAndCategory
                , taskListView dropId Later "Later" allTasksWithIndexAndCategory
                ]
            , div [ id "footer" ]
                [ buildInfoView buildInfo
                ]
            ]


taskListView : Maybe DragDropIndex -> TaskCategory -> String -> List ( Int, TaskCategory, Task ) -> Html Msg
taskListView maybeDropId category title allTasks =
    let
        tasksInThisCategory =
            List.filter (\( _, taskCategory, t ) -> taskCategory == category) allTasks

        taskViews =
            (List.map (\( index, _, task ) -> taskView maybeDropId category index task) tasksInThisCategory) ++ [ fakeDropView maybeDropId category ((List.length tasksInThisCategory) + 1) ]
    in
        div []
            [ h2 [] [ text title ]
            , ul [ class "tasks" ] taskViews
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
