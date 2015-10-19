import String

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)
import ElmTest.Test exposing (test, suite, Test)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Console exposing (runDisplay)

import Dict exposing (Dict)

import LSystem exposing (..)

-- Pythagoras Tree encoding
lsystem : LSystem
lsystem =
    { axiom = [ '0' ],
      rules = Dict.fromList [ ('1', [ '1', '1' ]),
                              ('0', [ '1', '[', '0', ']', '0' ])
                            ]
    }


t1 = test "Rules application"
     ( let s = applyRules lsystem.rules '1'
       in
         assertEqual s [ '1', '1' ]
     )

t2 = test "create gen zero"
     ( let zero = generation 0 lsystem
       in  assertEqual lsystem zero )

t3 = test "expected generation"
     ( let gen = generation 3 lsystem
           hash = String.fromList gen.axiom
       in  assertEqual hash "1111[11[1[0]0]1[0]0]11[1[0]0]1[0]0" )


tests : Test
tests = suite "A Test Suite"
        [ t1
        , t2
        , t3
        ]

port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Response
