module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)


rootView : Model -> Html Msg
rootView { buildInfo } =
    div [ id "content" ]
        [ h1 [ id "title" ] [ text "Asana Inbox" ]
        , div [ id "body" ]
            []
        , div [ id "footer" ]
            [ buildInfoView buildInfo
            ]
        ]


buildInfoView : BuildInfo -> Html Msg
buildInfoView buildInfo =
    text ("Version: " ++ buildInfo.time ++ " " ++ (String.slice 0 8 buildInfo.version) ++ "-" ++ buildInfo.tier)
