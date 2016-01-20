module SpinSquare where

import Time exposing (Time, second)
import Easing exposing (..)
import Effects exposing (..)

import Svg exposing (svg, rect, g, text, text')
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

import Html exposing (Html)

import StartApp
import Task

type alias Model =
  { angle : Float
  , animationState : AnimationState
  }

type alias AnimationState = Maybe
  { prevClockTime : Time
  , elapsedTime : Time
  }

init = (Model 0.0 Nothing, Effects.none)

rotationStep = 90
duration = second

type Action = Spin
            | Tick Time


update : Action -> Model -> (Model, Effects Action)
update act model =
  case act of
    Spin ->
      case model.animationState of
        Nothing ->
          ( model, Effects.tick Tick )
        Just _ ->
          ( model, Effects.none )
    Tick clockTime ->
      let newElapsedTime =
            case model.animationState of
              Nothing ->
                0
              Just { prevClockTime, elapsedTime } ->
                elapsedTime + ( clockTime - prevClockTime )
      in
        if newElapsedTime > duration then
          ( { angle = model.angle + rotationStep
            , animationState = Nothing
            }
          , Effects.none )
        else
          ( { angle = model.angle
            , animationState = Just { elapsedTime = newElapsedTime
                                    , prevClockTime = clockTime}
            }
          , Effects.tick Tick )

-- VIEW

toOffset : AnimationState -> Float
toOffset state =
  case state of
    Nothing ->
      0
    Just { elapsedTime } ->
      ease easeOutBounce float 0 rotationStep duration elapsedTime

view : Signal.Address Action -> Model -> Html
view address model =
  let angle = toOffset model.animationState
  in svg
       [ width "200", height "200", viewBox "0 0 200 200"]
       [ g [ transform ("translate(100, 100) rotate(" ++ toString angle ++ ")")
           , onClick (Signal.message address Spin)
           ]
         [ rect
           [ x "-50"
           , y "-50"
           , width "100"
           , height "100"
           , rx "15"
           , ry "15"
           , style "fill: #60B5CC;"
           ]
           []
         , text' [ fill "white", textAnchor "middle" ] [ text "Click me!" ]
         ]
       ]


--  APPLICATION

app = StartApp.start
      { init = init
      , update = update
      , view = view
      , inputs = []
      }

main : Signal Html
main = app.html

port task : Signal (Task.Task Never ())
port task = app.tasks
