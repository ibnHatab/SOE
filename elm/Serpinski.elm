module Serpinski (..) where

import Keyboard
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
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

init : Float -> Float -> Turtle.State
init x y =
  let startPos = { x = x, y = y }
      startDir = { l = 0.0, a = 0.0 }
  in { cur = { pos = startPos, dir = startDir }, stack = []}

t : Turtle.Translation
t = [ ('A', [ fwd 1])
    , ('B', [ fwd 1])
    , ('<', [ lft 60.0])
    , ('>', [ rgt 60.0]) ] |> Dict.fromList


display : (Int,Int) -> Int -> Element
display (w,h) gen =
  let start = init 0.0 0.0 -- (-(toFloat h)/2.0)
      sys = LSystem.generation gen serpinski
      paths = toTurtle start t sys.axiom
      forms =
        paths
          |> List.map (\(s, e) -> segment (s.x, s.y) (e.x, e.y))
          |> List.map (traced (solid black))
  in collage w h forms

generations : Signal Int
generations =
  foldp (\x n -> n + x |> max 0) 0 genChanges

genChanges : Signal Int
genChanges = .x <~ Keyboard.arrows

main : Signal Element
main = display <~ Window.dimensions ~ generations
