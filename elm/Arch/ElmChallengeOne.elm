module ElmChallengeOne where

import Mouse
import Signal
import Window
import Text exposing (fromString)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


view (x,y) (w,h) =
  let
    posLabel = if toFloat x < toFloat w/2 then "Left" else "Right"
  in
    collage w h
           [
            text (fromString posLabel)
           ]



main = Signal.map2 view  Mouse.position Window.dimensions
