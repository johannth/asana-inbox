module Api exposing (..)

import Date exposing (Date)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format, isoDateFormat)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (..)


apiUrl : String -> String -> String
apiUrl apiHost path =
    apiHost ++ path


requestHeaders : List AsanaAccessToken -> List Http.Header
requestHeaders accessTokens =
    [ Http.header "Authorization" ("Bearer " ++ String.join "," (List.map .token accessTokens)) ]


getJson : List AsanaAccessToken -> String -> Decode.Decoder a -> Http.Request a
getJson accessTokens url decoder =
    Http.request
        { method = "GET"
        , url = url
        , headers = requestHeaders accessTokens
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


postJson : List AsanaAccessToken -> String -> Http.Body -> Decode.Decoder a -> Http.Request a
postJson accessTokens url body decoder =
    Http.request
        { method = "POST"
        , url = url
        , headers = requestHeaders accessTokens
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


getTasks : String -> List AsanaAccessToken -> Cmd Msg
getTasks apiHost accessTokens =
    Http.send LoadTasks <|
        getJson accessTokens (apiUrl apiHost "/api/tasks") (Decode.field "tasks" decodeListOfTasks)


createTask : String -> List AsanaAccessToken -> AsanaTask -> Cmd Msg
createTask apiHost accessTokens task =
    Http.send (TaskCreated task.id) <|
        postJson accessTokens (apiUrl apiHost "/api/tasks/" ++ task.workspace.id ++ "/") (Http.jsonBody (encodeAsanaTaskForCreation task)) (Decode.field "task" decodeAsanaTask)


updateTask : String -> List AsanaAccessToken -> AsanaTask -> Cmd Msg
updateTask apiHost accessTokens task =
    Http.send TaskUpdated <|
        postJson accessTokens (apiUrl apiHost "/api/tasks/" ++ task.workspace.id ++ "/" ++ task.id) (Http.jsonBody (encodeAsanaTaskForUpdate task)) (Decode.succeed ())


decodeListOfTasks : Decode.Decoder (List AsanaTask)
decodeListOfTasks =
    Decode.list decodeAsanaTask


decodeAsanaTask : Decode.Decoder AsanaTask
decodeAsanaTask =
    Decode.map8 AsanaTask
        (Decode.field "id" Decode.string)
        (Decode.field "url" Decode.string)
        (Decode.field "assigneeStatus" decodeAssigneeStatus)
        (Decode.field "projects" (Decode.list decodeAsanaProject))
        (Decode.field "workspace" decodeAsanaWorkspace)
        (Decode.field "name" Decode.string)
        (Decode.field "dueOn" decodeDueOn)
        (Decode.succeed False)


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


encodeAsanaTaskForCreation : AsanaTask -> Encode.Value
encodeAsanaTaskForCreation task =
    Encode.object
        [ ( "name", Encode.string task.name )
        , ( "assigneeStatus", encodeAssigneeStatus task.assigneeStatus )
        ]


encodeAsanaTaskForUpdate : AsanaTask -> Encode.Value
encodeAsanaTaskForUpdate task =
    Encode.object
        ([ ( "completed", Encode.bool task.completed )
         , ( "assigneeStatus", encodeAssigneeStatus task.assigneeStatus )
         , ( "name", Encode.string task.name )
         ]
            ++ Maybe.withDefault []
                (Maybe.map
                    (\dueOn ->
                        [ ( "dueOn"
                          , Encode.string (format config isoDateFormat dueOn)
                          )
                        ]
                    )
                    task.dueOn
                )
        )


encodeAssigneeStatus : AssigneeStatus -> Encode.Value
encodeAssigneeStatus assigneeStatus =
    case assigneeStatus of
        Today ->
            Encode.string "today"

        New ->
            -- It's not accepted to create a task with assigneeStatus == new
            Encode.string "today"

        Upcoming ->
            Encode.string "upcoming"

        Later ->
            Encode.string "later"


decodeAccessTokens : Decode.Decoder (List AsanaAccessToken)
decodeAccessTokens =
    Decode.list decodeAccessToken


decodeAccessToken : Decode.Decoder AsanaAccessToken
decodeAccessToken =
    Decode.map2 AsanaAccessToken
        (Decode.field "name" Decode.string)
        (Decode.field "token" Decode.string)


encodeAccessTokens : List AsanaAccessToken -> Encode.Value
encodeAccessTokens tokens =
    Encode.list (List.map encodeAccessToken tokens)


encodeAccessToken : AsanaAccessToken -> Encode.Value
encodeAccessToken token =
    Encode.object [ ( "name", Encode.string token.name ), ( "token", Encode.string token.token ) ]


encodeTaskList : TaskList -> Encode.Value
encodeTaskList taskList =
    let
        encodeStringList =
            List.map Encode.string >> Encode.list
    in
    Encode.object
        [ ( "new", encodeStringList taskList.new )
        , ( "today", encodeStringList taskList.today )
        , ( "upcoming", encodeStringList taskList.upcoming )
        , ( "later", encodeStringList taskList.later )
        ]


decodeTaskList : Decode.Decoder TaskList
decodeTaskList =
    Decode.map4 TaskList
        (Decode.field "new" (Decode.list Decode.string))
        (Decode.field "today" (Decode.list Decode.string))
        (Decode.field "upcoming" (Decode.list Decode.string))
        (Decode.field "later" (Decode.list Decode.string))
