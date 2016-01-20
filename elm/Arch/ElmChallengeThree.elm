module ElmChallengeThree where

import Color exposing (Color, blue)
import Graphics.Collage exposing (Form, circle, collage, filled, move)
import Graphics.Element exposing (Element, layers)

import Time exposing (Time, second)
import Effects exposing (..)
import Random exposing (..)

import StartApp
import Task
import Keyboard
import Char

import Html exposing (Html, div, fromElement)
import Html.Attributes exposing (style)

import Time exposing (..)
import Signal exposing (..)

import Debug

-- Random circle
type alias CircleSpec = (Float, (Float,Float))

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

circleSpec : (Float, Float) -> Random.Generator CircleSpec
circleSpec (w,h) =
  Random.customGenerator <| \seed ->
    let (r, seed') = Random.generate (radius 20) seed
        ((x, y), seed'') = Random.generate (possition (w,h)) seed'
    in ((r, (x,y)), seed'')

-- MODEL

type alias Model =
  { circleSpecs : List CircleSpec
  , paused : Bool
  , scene : (Int,Int)
  }

init = ( Model [] False (400,400), Effects.none )

type Action = NoOp
            | Pause
            | Reset
            | Tick Time

update : Action -> Model -> (Model, Effects Action)
update act model =
  case act of
    NoOp ->
      ( model, Effects.none )
    Pause ->
      ( { model | paused <- not model.paused }, Effects.none )
    Reset ->
      ( { model | circleSpecs <- [] }, Effects.none )
    Tick seedTime ->
      if model.paused then
        ( model, Effects.none )
      else
        let timeSeed = Random.initialSeed (round seedTime)
            makeCircle seed (w,h) =
              Random.generate (circleSpec (toFloat w, toFloat h)) seed |> fst
            newCircleSpec = makeCircle timeSeed model.scene |> Debug.log "new"
        in ( { model | circleSpecs <- newCircleSpec :: model.circleSpecs }
           , Effects.none
           )

-- VIEW

drawCircle : CircleSpec -> Form
drawCircle (r, (x,y)) =
  filled blue (circle r) |>
  move (x, y)

(=>) = (,)
view : Signal.Address Action -> Model -> Html
view address model =
  div [style [ "width" => "400px" ]]
      [
       fromElement <| collage 400 400 (List.map drawCircle model.circleSpecs)
      ]

-- APPLICATION

timeSeed = Signal.map Tick (Time.every second)

keyPress = Signal.filterMap (\key ->
                               case Char.fromCode key |> Debug.log "keys" of
                                 'p' ->
                                   Just Pause
                                 'r' ->
                                   Just Reset
                                 otherwise ->
                                   Nothing
                        ) NoOp Keyboard.presses

app = StartApp.start
      { init = init
      , update = update
      , view = view
      , inputs = [timeSeed, keyPress]
      }

main : Signal Html
main = app.html

port task : Signal (Task.Task Never ())
port task = app.tasks
