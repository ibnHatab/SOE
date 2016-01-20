module Arch.Ship where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)

import Graphics.Element exposing (..)
import Graphics.Input exposing (..)

import Task exposing (..)
import Signal exposing (..)


type Side = Forward | Rear
type Action = NoOp
            | Shield Side Bool

type alias Model = { forward : Bool
                   , rear : Bool }

type alias Transformation = Model -> Model


fwd0 = True
rear0 = False

init : Model
init = Model fwd0 rear0


mb = Signal.mailbox NoOp

trans : Signal.Mailbox Transformation
trans = Signal.mailbox identity


cbForward v =
  checkbox
  (\b -> Signal.message mb.address (Shield Forward b))
  v

cbRear v =
  checkbox
  (Signal.message (Signal.forwardTo mb.address (Shield Rear)))
  v

cbReset =
  Html.button
  [
   onClick trans.address (\m -> { m | forward <- fwd0, rear <- rear0})
  ]
  [ text "Reset" ]


step : Action -> Model -> Model
step a m =
  case a of
    NoOp -> m
    Shield Forward b -> { m | forward <- b}
    Shield Rear b -> { m | rear <- b}


model0 = Signal.foldp step init mb.signal
model1 = Signal.foldp (<|) init trans.signal

model : Signal Model
model = Signal.merge model1 model0

scene m =
  flow down
       [ flow right [cbForward m.forward, show Forward]
       , flow right [cbRear m.rear, show Rear]
       , toElement 20 20 cbReset
       , show m
       ]

main = Signal.map scene model
