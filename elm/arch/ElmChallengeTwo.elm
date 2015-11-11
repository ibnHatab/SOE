module ElmChallengeTwo where

import Color exposing (Color, blue)
import Graphics.Collage exposing (Form, circle, collage, filled, move)
import Graphics.Element exposing (Element, layers)

import Random exposing (..)
import Signal exposing (..)
import Time exposing (..)

import Window

-- Random circle
radius : Float -> Random.Generator Float
radius rmax =
  Random.customGenerator <| \seed ->
    let (r, seed') = Random.generate (float 0 rmax) seed
    in (r, seed')

possition : (Float, Float) -> Random.Generator (Float, Float)
possition (w,h) =
  Random.customGenerator <| \seed ->
    let (x, seed') = Random.generate (float (-w/2) (w/2)) seed
        (y, seed'') = Random.generate (float (-h/2) (h/2)) seed'
    in ((x,y), seed'')

type alias CircleSpec = (Float, (Float,Float))

circleSpec : (Float, Float) -> Random.Generator CircleSpec
circleSpec (w,h) =
  Random.customGenerator <| \seed ->
    let (r, seed') = Random.generate (radius 20) seed
        ((x, y), seed'') = Random.generate (possition (w,h)) seed'
    in ((r, (x,y)), seed'')


-- SIGNALS
timeSeed : Signal Seed
timeSeed = Random.initialSeed << round <~ Time.every second

newCircleSignal : Signal CircleSpec
newCircleSignal =
  let makeCircle seed (w,h) =
        Random.generate (circleSpec (toFloat w, toFloat h)) seed |> fst
  in makeCircle <~ timeSeed ~ Window.dimensions

allCirclesSpecSignal :
        Signal (List CircleSpec)
allCirclesSpecSignal =
    foldp (::) [] newCircleSignal

-- VIEW
drawCircle : CircleSpec -> Form
drawCircle (r, (x,y)) =
  filled blue (circle r) |>
  move (x, y)

view : (Int, Int) -> List CircleSpec -> Element
view (w, h) circles = collage w h (List.map drawCircle circles)

main : Signal Element
main = view <~ Window.dimensions ~ allCirclesSpecSignal
