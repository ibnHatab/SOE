import String

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)
import ElmTest.Test exposing (test, suite, Test)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Console exposing (runDisplay)

import Dict exposing (Dict)

import LSystem exposing (..)

lSystem : LSystem
lSystem =
    { axiom = [ '0' ],
      rules = Dict.fromList [ ('1', [ '1', '1' ]),
                              ('0', [ '1', '[', '0', ']', '0' ])
                            ]
    }


t1 = test "Rules application"
     ( let s = applyRules lSystem.rules '1'
       in
         assertEqual s [ '1', '1' ]
     )
-- t2 = test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
-- t3 = test "This test should fail" (assert True)

tests : Test
tests = suite "A Test Suite"
        [ t1
        -- , t2
        -- , t3
        ]

port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Response
