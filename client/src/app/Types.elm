module Types exposing (..)

import Http
import Dict exposing (Dict)
import Date exposing (Date)
import Dom
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)
import DatePicker
import LocalStorage


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
    }


type AsanaTaskMutation
    = Complete
    | UpdateAssigneeStatus AssigneeStatus
    | UpdateDueOn Date
    | UpdateName String


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


type alias TaskListIndex =
    ( AssigneeStatus, String, Int )


type alias Model =
    { apiHost : String
    , today : Date
    , accessTokenFormExpanded : Bool
    , newAccessTokenName : String
    , newAccessTokenToken : String
    , accessTokens : List AsanaAccessToken
    , tasks : Dict String AsanaTask
    , taskList : Array String
    , workspaces : Dict String AsanaWorkspace
    , defaultWorkspace : Maybe String
    , buildInfo : BuildInfo
    , dragDrop : DragDrop.Model TaskListIndex TaskListIndex
    , expanded : ExpandedState
    , datePickers : Dict String DatePicker.DatePicker
    , expandedAssigneeStatusOverlay : Maybe String
    }


type Msg
    = Void
    | UrlChange Navigation.Location
    | DragDropMsg (DragDrop.Msg TaskListIndex TaskListIndex)
    | LoadTasks (Result Http.Error (List AsanaTask))
    | TaskCreated String (Result Http.Error AsanaTask)
    | TaskUpdated (Result Http.Error ())
    | ToggleExpanded AssigneeStatus
    | CompleteTask String
    | EditTaskName String String
    | StopEditTaskName String
    | AddNewTask TaskListIndex
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
