module Proofs where

import Data.Bits

parts :: String -> (Integer, Float)
parts ds = (ipart es, fpart fs)
  where (es, _:fs) = break (== '.') ds

ipart = foldl shiftl 0 . map toDigit
  where shiftl n d = n*10 + d

toDigit d = fromIntegral (fromEnum d - fromEnum '0')

fpart = foldr shiftr 0 . map toDigit
  where shiftr n d = (n + d) / 10


-- scanl' :: (a -> b -> b) -> b -> [a] -> [b]
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f e = map (foldl f e) . inits

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

scanl'' :: (b -> a -> b) -> b -> [a] -> [b]
scanl'' f e [] = [e]
scanl'' f e (x:xs) = e : scanl'' f (f e x) xs

mss = maximum . map sum . segments

segments = concat . map inits . tails

tails [] = [[]]
tails xs'@(x:xs) = xs' : (tails xs)

mss' = -- maximum .
  scanr (<>) 0
  where x <> y = 0 `max` (x+y)

-- cp [] = [[]]
-- cp (xs:xss) = [x:ys | x <- xs, ys <- xss]

cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

---------------- Accumulating parameters
data GenTree a = Node a [GenTree a] deriving Show

t = Node "a" [Node "ab" [], Node "ac" [Node "aca" [], Node "acb" []]]

labels :: GenTree a -> [a]
labels (Node x ts) = x : concat (map labels ts)

labcat :: [GenTree a] -> [a] -> [a]
labcat ts xs = concat (map labels ts) ++ xs


labels' :: GenTree a -> [a]
labels' ts = labcat' [ts] []

labcat' :: [GenTree a] -> [a] -> [a]
labcat' [] xs = xs
labcat' (Node x us:vs) xs = x:labcat' us (labcat' vs xs)

---------------------------------- Tupling

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib2 n = (fib n, fib (n+1))

fib2' 0 = (0,1)
fib2' n = (b, a+b) where (a, b) = fib2' (n-1)


----------------------------------- Hilbert

hilbertDistance d (x,y)
  | x < 0 || x >= 1 `shiftL` d = error "x bounds"
  | y < 0 || y >= 1 `shiftL` d = error "y bounds"
  | otherwise = dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y
  where dist 0 _ result _ _ = result
        dist side area result x y =
          case (compare x side, compare y side) of
          (LT, LT) -> step result y x
          (LT, _)  -> step (result + area) x (y - side)
          (_, LT)  -> step (result + area * 3) (side - y - 1) (side * 2 - x - 1)
          (_, _)   -> step (result + area * 2) (x - side) (y - side)
         where step = dist (side `shiftR` 1) (area `shiftR` 2)


hilbertPoint d n
  | n >= 1 `shiftL` (2*d) = error "x bounds"
  | otherwise = poin (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) (0, 0) n
  where poin 0 _ (x, y) n = (x, y)
        poin side area (x, y) n =
          case n `divMod` area  of
          (0, rest) -> step (x, y) rest
          (1, rest) -> step (x, y+side) rest
          (2, rest) -> step (x+side, y+side) rest
          (3, rest) -> step (x+side, y) rest
          where step = poin (side `shiftR` 1) (area `shiftR` 2)
