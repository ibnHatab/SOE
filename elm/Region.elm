module Region (..) where

import List

import Shape exposing (..)

type alias Vector = (Float, Float)

type Region = RShape Shape               -- primiv shape
            | Translate Vector Region -- translated region
            | Scale Vector Region
            | Complement Region         -- inverse of region
            | Union Region Region
            | Intersect Region Region   -- intersection of regions
            | Xor Region Region         -- XOR of regions
            | Empty                     -- empty region

infixr 5 `union`
infixr 6 `intersect`

union : Region -> Region -> Region
s1 `union` s2 = Union s1 s2

intersect : Region -> Region -> Region
s1 `intersect` s2 = Intersect s1 s2

univ : Region
univ = Complement Empty

type alias Coordinate = (Float,Float)
type alias Ray = (Coordinate, Coordinate)

isLeftOf : Coordinate -> Ray -> Bool
isLeftOf (px,py) ((ax,ay),(bx,by))
  = let (s,t) = (px-ax, py-ay)
        (u,v) = (px-bx, py-by)
    in s*v >= t*u

containsS : Shape -> Coordinate -> Bool
containsS s (x,y) =
  case s of
    (Rectangle s1 s2) ->
      let t1 = s1/2
          t2 = s2/2
      in -t1<=x && x<=t1 && -t2<=y && y<=t2
    (Ellipse r1 r2) ->
      (x/r1)^2 + (y/r2)^2 <= 1
    (Polygon pts) ->
      let rotate (x::xs) = xs ++ [x]
          zip = List.map2 (,)
          isLeftOfp p' = isLeftOf (x,y) p'
      in List.all isLeftOfp (zip pts (rotate pts))
    (RtTriangle s1 s2) ->
      (Polygon [(0,0),(s1,0),(0,s2)]) `containsS` (x,y)

containsR : Region -> Coordinate -> Bool
containsR s (x,y) =
  case s of
    (RShape s) ->
      s `containsS` (x,y)
    (Translate (u,v) r) ->
      r `containsR` (x-u, y-v)
    (Scale (u,v) r) ->
      r `containsR` (x/u,y/v)
    (Complement r) ->
      not (r `containsR` (x,y))
    (Union r1 r2) ->
      r1 `containsR` (x,y) || r2 `containsR` (x,y)
    (Intersect r1 r2) ->
      r1 `containsR` (x,y) && r2 `containsR` (x,y)
    Empty ->
      False
