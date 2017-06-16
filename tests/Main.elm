port module Main exposing (..)

import Json.Encode exposing (Value)
import Test exposing (..)
import Test.Runner.Node exposing (TestProgram, run)
import Test.Bytes as Bytes


tests : Test
tests =
    describe "Tests for Bytes type representation in Elm"
        [ Bytes.tests
        ]


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
