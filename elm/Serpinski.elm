module Serpinski (..) where

import Keyboard
import Graphics.Element exposing (..)
import Signal exposing (..)
import Window

import Dict exposing (Dict)
import LSystem exposing (LSystem)
import Turtle exposing (..)


serpinski : LSystem
serpinski =
  { axiom = [ 'A' ],
    rules = [ ('A', [ 'B', '>', 'A', '>', 'B' ])
            , ('B', [ 'A', '<', 'B', '<', 'A' ]) ] |> Dict.fromList
  }

init : Turtle.State
init =
  let startPos = { x = 0, y = 0 }
      startDir = { l = 0.0, a = 0.0 }
  in { cur = { pos = startPos, dir = startDir }, stack = []}

t : Turtle.Translation
t = [ ('A', [ fwd 1])
    , ('B', [ fwd 1])
    , ('<', [ lft 60.0])
    , ('>', [ rgt 60.0]) ] |> Dict.fromList


generations : Signal Int
generations =
  foldp (\x n -> n + x |> max 0) 0 genChanges

genChanges : Signal Int
genChanges = .x <~ Keyboard.arrows

main : Signal Element
main = (display init serpinski t) <~ Window.dimensions ~ generations
