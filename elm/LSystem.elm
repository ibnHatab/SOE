module LSystem (..) where

import Graphics.Element exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)

import Lazy.List exposing (LazyList, (:::), (+++), empty)
import Lazy exposing (Lazy, force, lazy)
import List
import Maybe

type alias Symbol = Char
type alias State = List Symbol
type alias Rules = Dict Symbol State

type alias LSystem = { axiom : State
                     , rules : Rules }

type alias Shrinker a = a -> LazyList a


-- Growing from original axion by applying the rules
applyRules : Rules -> Symbol -> State
applyRules rs s =
  case Dict.get s rs of
    Nothing ->    [s]
    Just x  ->     x


evolve : Shrinker LSystem
evolve ls =
  let srinker symbol =
        { ls | axiom <- applyRules ls.rules symbol }
  in
    Lazy.List.map (srinker) (Lazy.List.fromList ls.axiom)


-- forward : LazyList LSystem -> LazyList LSystem
forward lls =
  case Lazy.List.head lls of
    Nothing -> Nothing
    Just ls -> ls

generation gen ls =
  evolve ls |> Lazy.List.drop gen


-- Pythagoras Tree encoding
lsystem =
    { axiom = [ '0' ],
      rules = Dict.fromList [ ('1', [ '1', '1' ]),
                              ('0', [ '1', '[', '0', ']', '0' ])
                            ]
    }


-- forward : Rules -> LazyList State -> (State, LazyList State)
-- forward rs ls =
--   let init = g.axiom
--       gen = evolve g.rules
--   in
--     init |>

-- init : LSystem -> LazyList State
-- init = forward


--   List.map (\c -> applyRules c rs) s


-- main = show lSystem.rules
