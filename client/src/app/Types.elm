module Types exposing (..)

import Dict exposing (Dict)
import Http
import Date exposing (Date)
import Navigation


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


type alias Model =
    { apiHost : String
    , tasks : Dict String Task
    , newTasks : List String
    , todayTasks : List String
    , upcomingTasks : List String
    , laterTasks : List String
    , buildInfo : BuildInfo
    }


type Msg
    = Void
    | UrlChange Navigation.Location


mockProject1 =
    Project "p1" "Project 1"


mockProject2 =
    Project "p2" "Project 2"


mockProjects =
    Dict.fromList [ ( mockProject1.id, mockProject1 ), ( mockProject2.id, mockProject2 ) ]


mockTask1 =
    Task "1" (Just mockProject1) "A simple task" Nothing


mockTask2 =
    Task "2" (Just mockProject2) "A complex task" Nothing


mockTask3 =
    Task "3" Nothing "A task without a project" Nothing


mockTasks =
    Dict.fromList [ ( mockTask1.id, mockTask1 ), ( mockTask2.id, mockTask2 ), ( mockTask3.id, mockTask3 ) ]


emptyModel : Flags -> Model
emptyModel flags =
    { apiHost = flags.apiHost
    , tasks = mockTasks
    , newTasks = [ mockTask3.id ]
    , todayTasks = [ mockTask1.id, mockTask2.id ]
    , upcomingTasks = []
    , laterTasks = []
    , buildInfo = BuildInfo flags.buildVersion flags.buildTime flags.buildTier
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
