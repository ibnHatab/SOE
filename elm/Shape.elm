module Shape ( Shape (Rectangle, Ellipse, RtTriangle, Polygon)
             , Radius, Side, Vertex
             , square, circle, distBetween, area ) where

type alias Radius = Float
type alias Side   = Float
type alias Vertex  = (Float,Float)

type Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon (List Vertex)

square : Float -> Shape
square s = Rectangle s s

circle : Float -> Shape
circle r = Ellipse r r

area : Shape -> Float
area shape =
  case shape of
    Rectangle  s1 s2   -> s1*s2
    RtTriangle s1 s2   -> s1*s2/2
    Ellipse    r1 r2   -> pi*r1*r2
    Polygon    (v1::vs)->  let polyArea vs =
                                 case vs of
                                   (v2::v3::vs') -> triArea v1 v2 v3 + polyArea (v3::vs')
                                   otherwise     -> 0
                           in polyArea vs

triArea : Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 =
  let a = distBetween v1 v2
      b = distBetween v2 v3
      c = distBetween v3 v1
      s = 0.5*(a+b+c)
  in sqrt (s*(s-a)*(s-b)*(s-c))

distBetween : Vertex -> Vertex -> Float
distBetween (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
