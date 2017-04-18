module Api exposing (..)

import Json.Decode as Decode
import Http
import Types exposing (..)
import Date exposing (Date)


apiUrl : String -> String -> String
apiUrl apiHost path =
    apiHost ++ path


getJson : List AsanaAccessToken -> String -> Decode.Decoder a -> Http.Request a
getJson accessTokens url decoder =
    Http.request
        { method = "GET"
        , url = url
        , headers = [ Http.header "Authorization" ("Bearer " ++ (String.join "," (List.map .token accessTokens))) ]
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


getTasks : String -> List AsanaAccessToken -> Cmd Msg
getTasks apiHost accessTokens =
    Http.send LoadTasks <|
        getJson accessTokens (apiUrl apiHost "/api/tasks") (Decode.field "tasks" decodeTaskList)


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


decodeAssigneeStatus : Decode.Decoder AssigneeStatus
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
