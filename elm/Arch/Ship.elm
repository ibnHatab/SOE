module Ship where

import Task exposing (..)


type Side = Forward | Rear
type Action = NoOp
            | Shield Side Bool

type alias Model = { forward : Bool
                   , rear : Bool }

init : Model
init = Model True False
