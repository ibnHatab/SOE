module Animation (..) where

import Graphics.Collage as C exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Time exposing (..)
import Signal exposing (Signal, (<~), (~), filter, foldp, sampleOn)
import Keyboard


import Shape  as S exposing (..)
import Region  exposing (..)
import Picture exposing (..)

{- An animation is a continuous, time-varying image. -}

type alias Animation a = Time -> a

picToGraphic : Picture -> Graphic
picToGraphic pic =
  case pic of
    (PRegion c r) ->
      drawRegion c r
    (Over p1 p2) ->
      picToGraphic p1 `overGraphic` picToGraphic p2
    EmptyPic ->
      emptyGraphic

emptyGraphic = []
overGraphic = (++)

-- MODEL
rubberBall : Animation S.Shape
rubberBall t = Ellipse (sin t) (cos t)

revolvingBall : Animation Region
revolvingBall t
  = let ball = RShape (Ellipse 0.2 0.2)
    in Translate (sin t, cos t) ball

planets : Animation Picture
planets t
  = let p1 = PRegion red (RShape (rubberBall t))
        p2 = PRegion yellow (revolvingBall t)
    in p1 `over` p2

init : Graphic
init = emptyGraphic

type alias Keys = { x : Int, y : Int }

update : (Time, Keys) -> Graphic -> Graphic
update (time, keys) animation
  = (planets (time)) |> picToGraphic

animate : String -> Graphic -> Element
animate title graphic =
  layers
  [ collage 400 400 graphic
  , show title
  ]

-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = Signal.map (\(t, _) -> t /1000 )
              (timestamp (fps 30))
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

main : Signal Element
main = Signal.map (animate "Animated Region") (Signal.foldp update init inputSignal)
