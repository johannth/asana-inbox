module Types exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)
import Navigation
import Html5.DragDrop as DragDrop
import Array exposing (Array)


type alias Project =
    { id : String
    , name : String
    }


type alias Task =
    { id : String
    , project : Maybe Project
    , title : String
    , dueDate : Maybe Date
    }


type TaskCategory
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


type alias DragDropIndex =
    ( TaskCategory, Int )


type alias Model =
    { apiHost : String
    , tasks : Dict String Task
    , taskList : Array ( TaskCategory, String )
    , buildInfo : BuildInfo
    , dragDrop : DragDrop.Model DragDropIndex DragDropIndex
    , expanded : ExpandedState
    }


type Msg
    = Void
    | UrlChange Navigation.Location
    | DragDropMsg (DragDrop.Msg DragDropIndex DragDropIndex)
    | ToggleExpanded TaskCategory


mockProject1 =
    Project "p1" "Project 1"


mockProject2 =
    Project "p2" "Project 2"


mockProjects =
    Dict.fromList [ ( mockProject1.id, mockProject1 ), ( mockProject2.id, mockProject2 ) ]


mockTask1 =
    Task "1" (Just mockProject1) "Task 1" Nothing


mockTask2 =
    Task "2" (Just mockProject2) "Task 2" Nothing


mockTask3 =
    Task "3" Nothing "Task 3" Nothing


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
