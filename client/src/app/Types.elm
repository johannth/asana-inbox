module Types exposing (..)

import Date exposing (Date)
import DatePicker
import Dict exposing (Dict)
import Dom
import Html5.DragDrop as DragDrop
import Http
import LocalStorage
import Navigation


type alias AsanaAccessToken =
    { name : String
    , token : String
    }


type alias AsanaProject =
    { id : String
    , name : String
    }


type alias AsanaWorkspace =
    { id : String
    , name : String
    }


type alias AsanaTask =
    { id : String
    , url : String
    , assigneeStatus : AssigneeStatus
    , projects : List AsanaProject
    , workspace : AsanaWorkspace
    , name : String
    , dueOn : Maybe Date
    , completed : Bool
    }


type AssigneeStatus
    = New
    | Today
    | Upcoming
    | Later


type alias ExpandedState =
    { new : Bool
    , today : Bool
    , upcoming : Bool
    , later : Bool
    }


type alias DragTarget =
    { assigneeStatus : AssigneeStatus
    , targetId : String
    }


type DropTarget
    = Before AssigneeStatus String
    | After AssigneeStatus String
    | End AssigneeStatus


equalDropTarget : DropTarget -> DropTarget -> Bool
equalDropTarget a b =
    case ( a, b ) of
        ( Before assigneeStatusA targetIdA, Before assigneeStatusB targetIdB ) ->
            assigneeStatusA == assigneeStatusB && targetIdA == targetIdB

        ( After assigneeStatusA targetIdA, After assigneeStatusB targetIdB ) ->
            assigneeStatusA == assigneeStatusB && targetIdA == targetIdB

        ( End assigneeStatusA, End assigneeStatusB ) ->
            assigneeStatusA == assigneeStatusB

        _ ->
            False


assigneeStatusFromDropTarget : DropTarget -> AssigneeStatus
assigneeStatusFromDropTarget dropTarget =
    case dropTarget of
        Before assigneeStatus _ ->
            assigneeStatus

        After assigneeStatus _ ->
            assigneeStatus

        End assigneeStatus ->
            assigneeStatus


type alias TaskList =
    { new : List String
    , today : List String
    , upcoming : List String
    , later : List String
    }


type alias Model =
    { apiHost : String
    , today : Date
    , inPlanningMode : Bool
    , accessTokenFormExpanded : Bool
    , newAccessTokenName : String
    , newAccessTokenToken : String
    , accessTokens : List AsanaAccessToken
    , tasks : Dict String AsanaTask
    , taskList : Maybe TaskList
    , workspaces : Dict String AsanaWorkspace
    , defaultWorkspace : Maybe String
    , buildInfo : BuildInfo
    , dragDrop : DragDrop.Model DragTarget DropTarget
    , expanded : ExpandedState
    , datePickers : Dict String DatePicker.DatePicker
    , expandedAssigneeStatusOverlay : Maybe String
    }


type Msg
    = Void
    | UrlChange Navigation.Location
    | DragDropMsg (DragDrop.Msg DragTarget DropTarget)
    | LoadTasks (Result Http.Error (List AsanaTask))
    | TaskCreated String (Result Http.Error AsanaTask)
    | TaskUpdated (Result Http.Error ())
    | ToggleExpanded AssigneeStatus
    | CompleteTask String
    | EditTaskName String String
    | StopEditTaskName String
    | AddNewTask AssigneeStatus String
    | FocusResult (Result Dom.Error ())
    | RemoveAccessToken AsanaAccessToken
    | ToggleAccessTokenForm
    | AddAccessTokenName String
    | AddAccessTokenToken String
    | SaveAccessToken
    | ReceiveItem LocalStorage.LocalStorageItem
    | ToDatePicker String DatePicker.Msg
    | ToggleAssigneeStatusOverlay String
    | SetAssigneeStatus String AssigneeStatus
    | SetDefaultWorkspace String
    | TogglePlanningMode



-- BUILD INFO


type alias Flags =
    { apiHost : String
    , today : Float
    , buildVersion : String
    , buildTier : String
    , buildTime : String
    }


type alias BuildInfo =
    { version : String
    , time : String
    , tier : String
    }
