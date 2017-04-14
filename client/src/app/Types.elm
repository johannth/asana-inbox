module Types exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)
import Dom
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)
import DatePicker


type alias AsanaProject =
    { id : String
    , name : String
    }


type alias AsanaTask =
    { id : String
    , project : Maybe AsanaProject
    , title : String
    , dueDate : Maybe Date
    }


type AsanaTaskCategory
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
    ( AsanaTaskCategory, Int )


type alias Model =
    { apiHost : String
    , tasks : Dict String AsanaTask
    , taskList : Array ( AsanaTaskCategory, String )
    , buildInfo : BuildInfo
    , dragDrop : DragDrop.Model TaskListIndex TaskListIndex
    , expanded : ExpandedState
    , datePickers : Dict String DatePicker.DatePicker
    }


type Msg
    = Void
    | UrlChange Navigation.Location
    | DragDropMsg (DragDrop.Msg TaskListIndex TaskListIndex)
    | ToggleExpanded AsanaTaskCategory
    | CompleteTask String
    | EditTaskTitle String String
    | AddNewTask TaskListIndex
    | FocusResult (Result Dom.Error ())
    | ToDatePicker String DatePicker.Msg



-- BUILD INFO


type alias Flags =
    { apiHost : String
    , buildVersion : String
    , buildTier : String
    , buildTime : String
    }


type alias BuildInfo =
    { version : String
    , time : String
    , tier : String
    }
