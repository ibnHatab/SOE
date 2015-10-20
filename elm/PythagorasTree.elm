module PythagorasTree (..) where

import Keyboard
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Signal exposing (..)
import Window
import Debug

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

init : Float -> Float -> Turtle.State
init x y =
  let startPos = { x = x, y = y }
      startDir = { l = 10.0, a = 90.0 }
  in { cur = { pos = startPos, dir = startDir }, stack = []}

t : Turtle.Translation
t = [ ('0', [ fwd 3])
    , ('1', [ fwd 6])
    , ('[', [ Push, lft 45.0])
    , (']', [ Pop, rgt 45.0]) ] |> Dict.fromList

display : (Int,Int) -> Int -> Element
display (w,h) gen =
  let start = init 0.0 0.0 -- (-(toFloat h)/2.0)
      sys = LSystem.generation gen pTree
      paths = toTurtle start t (sys.axiom |> Debug.log "path")
      forms =
        paths |> List.map (\(s, e) -> segment (s.x, s.y) (e.x, e.y)
              |> (traced (solid black)) )

  in collage w h forms

generations : Signal Int
generations =
  foldp (\x n -> n + x |> max 0) 0 genChanges

genChanges : Signal Int
genChanges = .x <~ Keyboard.arrows

main : Signal Element
main = display <~ Window.dimensions ~ generations
