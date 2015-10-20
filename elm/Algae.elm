module Algae where

import LSystem exposing (..)

import Graphics.Element exposing (..)
import Dict
import Signal exposing (..)
import String
import Debug
import Time
import Keyboard

algae : LSystem
algae =
  { axiom = [ 'A' ],
    rules = Dict.fromList [
              ('A', [ 'A', 'B' ]),
              ('B', [ 'A' ])
            ] }

evolveTo : LSystem -> Int -> State
evolveTo ls gen =
  let cur = generation gen ls
  in cur.axiom

genChanges : Signal Int
genChanges = .x <~ Keyboard.arrows

generations : Signal Int
generations =
  foldp (\x n -> n + x |> max 0) 0 genChanges

states : LSystem -> Signal State
states lSys =
  (evolveTo lSys) <~ (Debug.watch "gen" <~ generations)

output = String.fromList <~ (states algae)
main = show <~ output
