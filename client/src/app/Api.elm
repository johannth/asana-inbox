module Api exposing (..)

import Json.Decode as Decode
import Dict exposing (Dict)
import Http
import Types exposing (..)
import Date exposing (Date)
import Set


apiUrl : String -> String -> String
apiUrl apiHost path =
    apiHost ++ path


getTasks : String -> Cmd Msg
getTasks apiHost =
    Http.send LoadTasks <|
        Http.get (apiUrl apiHost "/api/tasks") (Decode.field "tasks" decodeTaskList)


decodeTaskList : Decode.Decoder (List AsanaTask)
decodeTaskList =
    Decode.list decodeAsanaTask


decodeAsanaTask : Decode.Decoder AsanaTask
decodeAsanaTask =
    Decode.map6 AsanaTask
        (Decode.field "id" Decode.string)
        (Decode.field "assigneeStatus" decodeAssigneeStatus)
        (Decode.field "projects" (Decode.list decodeAsanaProject))
        (Decode.field "workspace" decodeAsanaWorkspace)
        (Decode.field "name" Decode.string)
        (Decode.field "dueOn" decodeDueOn)


decodeAsanaProject : Decode.Decoder AsanaProject
decodeAsanaProject =
    Decode.map2 AsanaProject
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


decodeAsanaWorkspace : Decode.Decoder AsanaWorkspace
decodeAsanaWorkspace =
    Decode.map2 AsanaWorkspace
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


decodeDueOn : Decode.Decoder (Maybe Date)
decodeDueOn =
    Decode.map
        (\maybeDateAsString ->
            case maybeDateAsString of
                Just dateAsString ->
                    case Date.fromString dateAsString of
                        Ok date ->
                            Just date

                        Err message ->
                            Nothing

                Nothing ->
                    Nothing
        )
        (Decode.maybe Decode.string)


decodeAssigneeStatus : Decode.Decoder AsanaTaskCategory
decodeAssigneeStatus =
    Decode.map
        (\value ->
            case value of
                "new" ->
                    New

                "later" ->
                    Later

                "today" ->
                    Today

                "upcoming" ->
                    Upcoming

                _ ->
                    New
        )
        Decode.string
