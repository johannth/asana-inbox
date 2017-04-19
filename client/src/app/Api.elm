module Api exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Types exposing (..)
import Date exposing (Date)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format, isoDateFormat)


apiUrl : String -> String -> String
apiUrl apiHost path =
    apiHost ++ path


requestHeaders : List AsanaAccessToken -> List Http.Header
requestHeaders accessTokens =
    [ Http.header "Authorization" ("Bearer " ++ (String.join "," (List.map .token accessTokens))) ]


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
        getJson accessTokens (apiUrl apiHost "/api/tasks") (Decode.field "tasks" decodeTaskList)


updateTask : String -> List AsanaAccessToken -> AsanaTask -> AsanaTaskMutation -> Cmd Msg
updateTask apiHost accessTokens task mutation =
    Http.send TaskUpdated <|
        postJson accessTokens (apiUrl apiHost "/api/tasks/" ++ task.workspace.id ++ "/" ++ task.id) (Http.jsonBody (encodeAsanaTaskMutation mutation)) (Decode.succeed ())


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


encodeAsanaTaskMutation : AsanaTaskMutation -> Encode.Value
encodeAsanaTaskMutation mutation =
    case mutation of
        Complete ->
            Encode.object [ ( "completed", Encode.bool True ) ]

        UpdateAssigneeStatus status ->
            Encode.object [ ( "assigneeStatus", encodeAssigneeStatus status ) ]

        UpdateDueOn dueDate ->
            Encode.object [ ( "dueOn", Encode.string (format config isoDateFormat dueDate) ) ]

        UpdateName name ->
            Encode.object [ ( "name", Encode.string name ) ]


encodeAssigneeStatus : AssigneeStatus -> Encode.Value
encodeAssigneeStatus assigneeStatus =
    case assigneeStatus of
        Today ->
            Encode.string "today"

        New ->
            Encode.string "new"

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
