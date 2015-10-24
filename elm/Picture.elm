module Picture (..) where

import Graphics.Collage as C exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Debug
import Mouse
import Window

import Region exposing (..)
import Shape exposing (..)

{-| Drawin Regions -}
type Picture = PRegion Color Region
             | Over Picture Picture
             | EmptyPic

over : Picture -> Picture -> Picture
p1 `over` p2 = Over p1 p2

drawPic : Picture -> List Form
drawPic p =
  case p of
    (Over p1 p2) ->
      (drawPic p2) ++ (drawPic p1)
    (PRegion c r) ->
      drawRegion c r
    EmptyPic ->
      []

drawRegion : Color -> Region -> List Form
drawRegion c r =
  (regionToGRegion r) |> List.map (filled c)

regionToGRegion : Region -> List C.Shape
regionToGRegion r = regToGReg (0,0) (1,1) r

regToGReg : Vector -> Vector -> Region -> List C.Shape
regToGReg (lx,ly) (sx,sy) region =
  let
    (loc, sca) = ((lx,ly), (sx,sy))
  in
    case region of
      (RShape s) ->
        [shapeToGRegion loc sca s]
      (Scale (u,v) r) ->
        regToGReg loc (sx*u,sy*v) r
      (Translate (u,v) r) ->
        regToGReg (lx+u*sx,ly+v*sy) (sx,sy) r
      Empty ->
        [rect 0 0]
      (Union r1 r2) ->
        let gr1 = regToGReg loc sca r1
            gr2 = regToGReg loc sca r2
        in gr1 ++ gr2
      {- Following are not srawable but used in interation -}
      (Intersect r1  r2) -> []
      (Xor r1 r2)        -> []
      (Complement  r)    -> []

{- Convert Shape to Collage.Shape with translation and scale -}
inchToPixel : Float -> Float
inchToPixel x = (72*x)

pixelToInch : Int -> Float
pixelToInch x = (toFloat x)/72

shapeToGRegion :
        Vector -> Vector -> Shape.Shape -> C.Shape
shapeToGRegion (lx,ly) (sx,sy) s =
  let trans (x,y) = ( inchToPixel(lx+x*sx), inchToPixel(ly+y*sy) )
      cshape = case s of
       Rectangle s1 s2 ->
         rect s1 s2
       Ellipse r1 r2 ->
         oval r1 r2
       Polygon vs ->
         polygon vs
       RtTriangle s1 s2 ->
         polygon [(0,0),(s1,0),(0,s2)]
  in
    List.map trans cshape

{- helper function for interactivity -}
pictToList : Picture -> List (Color,Region)
pictToList pic =
  case pic of
    EmptyPic      -> []
    (PRegion c r)  -> [(c,r)]
    (Over p1 p2) -> pictToList p1 ++ pictToList p2

{-| 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
first element is longest prefix (possibly empty) of @xs@ of elements that
/do not satisfy/ @p@ and second element is the remainder of the list -}
break : (a -> Bool) -> List a -> (List a, List a)
break p xs =
  case xs of
    [] -> (xs, xs)
    (x::xs') ->
      if p x then ([],xs)
      else let (ys,zs) = break p xs' in (x::ys,zs)

adjust : List (Color,Region) -> Coordinate ->
         (Maybe (Color,Region), List (Color,Region))
adjust regs p
  = case (break (\(_,r) -> r `containsR` p) regs) of
      (top,hit::rest) -> (Just hit, top++rest)
      (_,[])         -> (Nothing, regs)

-- tests
r1 = RShape (Rectangle 3 2)
r2 = RShape (Ellipse 1 1.5)
r3 = RShape (RtTriangle 3 2)
r4 = RShape (Polygon [(-2.5,2.5), (-3.0,0), (-1.7,-1.0),
                                 (-1.1,0.2), (-1.5,2.0)] )
p1 = PRegion red r1
p2 = PRegion blue r2
p3 = PRegion green r3
p4 = PRegion yellow r4

update : (Int, Int) -> Picture -> Picture
update (x, y) pic =
  let regs = pictToList pic
      (wx, wy) = (pixelToInch (x - 200), pixelToInch (200 - y))
      updatedRegs =
        case adjust regs (wx, wy) of
          (Nothing,  _      ) -> regs
          (Just hit, newRegs) -> (hit :: newRegs)
  in
    updatedRegs
    |> List.foldr (\(c, r) pics -> (PRegion c r) `over` pics) EmptyPic

pics : Picture
pics = List.foldl Over EmptyPic [p1,p2,p3,p4]

clickLocations : Signal Picture
clickLocations =
  Signal.foldp (update) pics (Signal.sampleOn Mouse.clicks Mouse.position)

scene : (Int, Int) -> Picture -> Element
scene (w,h) pic =
  layers
        [ collage 400 400 (drawPic pic)
        , show "Click to stamp a pentagon."
        ]

main : Signal Element
main =
  Signal.map2 scene Window.dimensions clickLocations
