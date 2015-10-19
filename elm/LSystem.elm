module LSystem (..) where

import Dict exposing (Dict)
import Lazy.List exposing (LazyList)

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

evolve : LSystem -> LSystem
evolve ls =
  let srinker symbol = applyRules ls.rules symbol
      newState = List.map (srinker) ls.axiom |> List.concat
  in
    { ls | axiom <- newState }

forward : Shrinker LSystem
forward ls =
  Lazy.List.iterate evolve ls

-- compute nth generation of lSystem
generation : Int -> LSystem -> LSystem
generation gen ls =
  ls
  |> forward
  |> Lazy.List.drop gen
  |> Lazy.List.head
  |> Maybe.withDefault ls
