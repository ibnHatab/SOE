module LSystem (..) where

import Graphics.Element exposing (..)
import Dict exposing (Dict)

type alias Symbol = Char
type alias State = List Symbol
type alias Rules = Dict Symbol State

type alias LSystem = { axiom : State
                     , rules : Rules }


lSystem =
    { axiom = [ '0' ],
      rules = Dict.fromList [ ('1', [ '1', '1' ]),
                              ('0', [ '1', '[', '0', ']', '0' ])
                            ]
    }

-- main = show lSystem.rules
