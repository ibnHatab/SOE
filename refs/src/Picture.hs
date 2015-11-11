module Picture ( Picture (Region, Over, EmptyPic),
                 Color (Black, Blue, Green, Cyan,
                        Red, Magenta, Yellow, White),
                 regionToGRegion, shapeToGRegion,
                 drawRegionInWindow, drawPic, draw, spaceClose, main,
                 module Region
               ) where
import Draw
import Region
import SOE hiding (Region)
import qualified SOE as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
             deriving Show

type Vector = (Float,Float)


draw :: String -> Picture -> IO ()
draw s p
  = runGraphics $
    do w <- openWindow s (xWin,yWin)
       loop w (pictToList p)

pictToList :: Picture -> [(Color,Region)]
pictToList  EmptyPic      = []
pictToList (Region c r)   = [(c,r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2


xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

loop :: Window -> [(Color,Region)] -> IO ()
loop w regs =
  do clearWindow w
     sequence_ [ drawRegionInWindow w c r | (c,r) <- reverse regs ]
     (x,y) <- getLBP w
     case (adjust regs (pixelToInch (x - xWin2),
                        pixelToInch (yWin2 - y) )) of
       (Nothing,  _      ) -> closeWindow w
       (Just hit, newRegs) -> loop w (hit : newRegs)


drawPic                 :: Window -> Picture -> IO ()
drawPic w (Region c r)   = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = do drawPic w p2; drawPic w p1
drawPic w EmptyPic       = return ()

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r
  = drawInWindow w
    (withColor c
     (drawRegion (regionToGRegion r)))


shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
shapeToGRegion (lx,ly) (sx,sy) s =
  case s of
  Rectangle s1 s2 ->
    createRectangle (trans (-s1/2,-s2/2)) (trans (s1/2,s2/2))
  Ellipse r1 r2 ->
    createEllipse (trans (-r1,-r2))(trans ( r1, r2))
  Polygon vs ->
    createPolygon (map trans vs)
  RtTriangle s1 s2 ->
    createPolygon (map trans [(0,0),(s1,0),(0,s2)])
   where trans :: Vertex -> Point
         trans (x,y) = ( xWin2 + inchToPixel (lx+x*sx),
                         yWin2 - inchToPixel (ly+y*sy) )

regionToGRegion :: Region -> G.Region
regionToGRegion = regToGReg (0,0) (1,1)

regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s)
  = shapeToGRegion loc sca s
regToGReg loc (sx,sy) (Scale (u,v) r)
  = regToGReg loc (sx*u,sy*v) r
regToGReg (lx,ly) (sx,sy) (Translate (u,v) r)
  = regToGReg (lx+u*sx,ly+v*sy) (sx,sy) r
regToGReg loc sca Empty
  = createRectangle (0,0) (0,0)
regToGReg loc sca (r1 `Union` r2)
  = primGReg loc sca r1 r2 orRegion
regToGReg loc sca (r1 `Intersect` r2)
  = primGReg loc sca r1 r2 andRegion
regToGReg loc sca (r1 `Xor` r2)
  = primGReg loc sca r1 r2 xorRegion
regToGReg loc sca (Complement  r)
  = primGReg loc sca winRect r diffRegion

primGReg loc sca r1 r2 op
  = let gr1 = regToGReg loc sca r1
        gr2 = regToGReg loc sca r2
    in  op gr1 gr2

winRect :: Region
winRect = Shape (Rectangle
                 (pixelToInch xWin) (pixelToInch yWin))


adjust :: [(Color,Region)] -> Coordinate ->
          (Maybe (Color,Region), [(Color,Region)])
adjust regs p
   = case (break (\(_,r) -> r `containsR` p) regs) of
       (top,hit:rest) -> (Just hit, top++rest)
       (_,[])         -> (Nothing, regs)

r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5,2.5), (-3.0,0), (-1.7,-1.0),
                     (-1.1,0.2), (-1.5,2.0)] )

p1,p2,p3,p4 :: Picture
p1 = Region Red r1
p2 = Region Blue r2
p3 = Region Green r3
p4 = Region Yellow r4

pic :: Picture
pic = foldl Over EmptyPic [p1,p2,p3,p4]

main = draw "Picture Click Test" pic
