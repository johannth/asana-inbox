module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Types exposing (..)


expandTasks : Dict String Task -> List String -> List Task
expandTasks allTasks taskIds =
    List.filterMap (\taskId -> Dict.get taskId allTasks) taskIds


rootView : Model -> Html Msg
rootView { newTasks, todayTasks, laterTasks, upcomingTasks, tasks, buildInfo } =
    let
        expandedNewTasks =
            expandTasks tasks newTasks

        expandedTodayTasks =
            expandTasks tasks todayTasks

        expandedLaterTasks =
            expandTasks tasks laterTasks

        expandedUpcomingTasks =
            expandTasks tasks upcomingTasks
    in
        div [ id "content" ]
            [ h1 [ id "title" ] [ text "Asana Inbox" ]
            , div [ id "body" ]
                [ taskList "New" expandedNewTasks
                , taskList "Today" expandedTodayTasks
                , taskList "Upcoming" expandedLaterTasks
                , taskList "Later" expandedUpcomingTasks
                ]
            , div [ id "footer" ]
                [ buildInfoView buildInfo
                ]
            ]


taskList : String -> List Task -> Html Msg
taskList title tasks =
    div []
        [ h2 [] [ text title ]
        , ul [] (List.map taskView tasks)
        ]


taskView : Task -> Html Msg
taskView task =
    li []
        [ text task.title
        ]


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ (String.slice 0 8 buildInfo.version) ++ "-" ++ buildInfo.tier)
