module Types exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)
import Dom
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)


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


mockProject1 =
    AsanaProject "p1" "Project 1"


mockProject2 =
    AsanaProject "p2" "Project 2"


mockProjects =
    Dict.fromList [ ( mockProject1.id, mockProject1 ), ( mockProject2.id, mockProject2 ) ]


mockTask1 =
    AsanaTask "0" (Just mockProject1) "AsanaTask 1" Nothing


mockTask2 =
    AsanaTask "1" (Just mockProject2) "AsanaTask 2" Nothing


mockTask3 =
    AsanaTask "2" Nothing "AsanaTask 3" Nothing


mockTasks =
    Dict.fromList [ ( mockTask1.id, mockTask1 ), ( mockTask2.id, mockTask2 ), ( mockTask3.id, mockTask3 ) ]


emptyModel : Flags -> Model
emptyModel flags =
    { apiHost = flags.apiHost
    , tasks = mockTasks
    , taskList = Array.fromList [ ( New, mockTask1.id ), ( Today, mockTask2.id ), ( Today, mockTask3.id ) ]
    , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
    , dragDrop = DragDrop.init
    , expanded = { today = True, new = True, upcoming = False, later = False }
    }



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
