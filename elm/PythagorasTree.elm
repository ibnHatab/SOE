module PythagorasTree (..) where

import Keyboard
import Graphics.Element exposing (..)
import Signal exposing (..)
import Window
-- import Debug

import Dict exposing (Dict)
import LSystem exposing (LSystem)
import Turtle exposing (..)


pTree : LSystem
pTree =
  { axiom = [ '0' ],
    rules = Dict.fromList [
              ('1', [ '1', '1' ]),
              ('0', [ '1', '[', '0', ']', '0' ])
            ] }

init : Turtle.State
init  =
  let startPos = { x = 0, y = 0 }
      startDir = { l = 10.0, a = 90.0 }
  in { cur = { pos = startPos, dir = startDir }, stack = []}

t : Turtle.Translation
t = [ ('0', [ fwd 3])
    , ('1', [ fwd 6])
    , ('[', [ Push, lft 45.0])
    , (']', [ Pop, rgt 45.0]) ] |> Dict.fromList


generations : Signal Int
generations =
  foldp (\x n -> n + x |> max 0) 0 genChanges

genChanges : Signal Int
genChanges = .x <~ Keyboard.arrows

main : Signal Element
main = (display init pTree t) <~ Window.dimensions ~ generations
