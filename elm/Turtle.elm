module Turtle (..) where

import LSystem exposing (..)
import Dict exposing (Dict)

-- Modelling the Turtle/Logo instructions
type alias Len = Float
type alias Deg = Float

-- Turtle instruction set
type Inst
  = Move Len
  | Turn Deg
  | Push
  | Pop


-- Transforming the L-system state into a sequence of lines to draw
type alias Pos = { x : Float, y : Float }
type alias Dir = { l : Len, a : Deg }
type alias Turtle = { pos : Pos, dir : Dir }

type alias State = {
    cur : Turtle
  , stack : List Turtle
  }

fwd : Len -> Inst
fwd x = Move x

lft : Deg -> Inst
lft x = Turn x

rgt : Deg -> Inst
rgt x = Turn (-x)

turn : Deg -> Turtle -> Turtle
turn angle turtle =
  let curDir = turtle.dir
  in { turtle | dir <- {curDir | a <- curDir.a + angle} }

type alias Translation = Dict Symbol (List Inst)

type alias Draw = (Pos, Pos)

line : Pos -> Len -> Deg -> Pos
line p l a =
  let r = a * pi / 180
  in { x = p.x + l * cos r, y = p.y + l * sin r}

execute : Inst -> State -> (Maybe Draw, State)
execute inst state =
  case inst of
    Push ->
      (Nothing, { state | stack <- (state.cur :: state.stack) })
    Pop ->
      case state.stack of
        h::tail -> (Nothing, { cur = h, stack = tail})
    Turn angle ->
      (Nothing, { state | cur <- turn angle state.cur })
    Move len ->
      let turtle = state.cur
          start = turtle.pos
          end = line start len turtle.dir.a
      in (Just (start, end), {state | cur <- { turtle | pos <- end }})
translate : Translation -> Symbol -> (List Inst)
translate ts x =
  case Dict.get x ts of
    Just inst -> inst

toTurtle : State -> Translation -> List Symbol -> List Draw
toTurtle init ts xs =
  xs
    |> List.map (translate ts)
    |> List.concat
    |> List.foldl (\inst (ops, state) ->
                     case execute inst state of
                       (Nothing, s) -> (ops, s)
                       (Just op, s) -> (op::ops, s)
                  ) ([], init)
    |> fst
