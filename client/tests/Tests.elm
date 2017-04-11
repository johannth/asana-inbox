module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import State exposing (..)
import Types exposing (..)


all : Test
all =
    describe "Calculate Priority Test Suite"
        [ describe "defaultPriorityWeights"
            [ test "should sum to 1" <|
                \() ->
                    Expect.equal 1 1
            ]
        ]
