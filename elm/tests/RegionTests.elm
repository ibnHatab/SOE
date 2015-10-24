import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)
import Debug
import ElmTest.Test exposing (suite)
import ElmTest.Runner.Console exposing (runDisplay)

import Check exposing (claim, that, for, is)
import Check.Test exposing (test)
import Check.Investigator exposing (..)
import Shrink exposing (map, andMap)
import Random.List
import Random.Extra exposing (map, andMap)

import Random exposing (initialSeed)
import Shape exposing (..)
import Region exposing (..)

oneCircle : Region
oneCircle = RShape (Ellipse 1 1)

fiveCircles : Region
fiveCircles = [0..5]
              |> List.map (\x -> Translate (x*2,0) oneCircle)
              |> List.foldr Union Empty


reverse ls = List.reverse (ls)

shape =
  let
    shrinker (RShape (Rectangle x y)) =
      (\ w h -> (RShape (Rectangle w h)))
      `Shrink.map`    shrink float x
      `Shrink.andMap` shrink float y

    generator =
      (\ w h -> (RShape (Rectangle w h)))
      `Random.Extra.map`    random float
      `Random.Extra.andMap` random float
  in
    investigator generator shrinker

tests = suite "Algebraic properties of region" [
         (test "Reversing a list twice yields the original list" -- claim
          (\list -> reverse (reverse list)) -- that
          (identity)                        -- is
          (list int)                        -- for Investigator
          100                               -- num of test
          (Random.initialSeed 42  |> Debug.log ">> Seed") -- seed
         )
        , ( test "Union is assotiative"
            -- claim that
            (\ (r1, r2, r3, x, y) ->
               (r1 `union` (r2 `union` r3))  `containsR` (x, y) )
            -- is
            (\ (r1, r2, r3, x, y) ->
               ((r1 `union` r2) `union` r3 )`containsR` (x, y) )
            -- for
            (tuple5 (shape, shape, shape, float, float))
            -- num of test
            1000
            -- with seed
            (Random.initialSeed 42)
          )
        ]

-- console runner
port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Response
