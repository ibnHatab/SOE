module TheBurrowsWheelerTransform where

import Data.List

transform :: Ord a => [a] -> ([a], Int)
transform xs = (map last xss, position xs xss)
  where xss = sort (rots xs)

position xs xss = length (takeWhile (<xs) xss)

rots :: Ord a => [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
  where lrot (x:xs) = xs ++ [x]

untransform :: Ord a => ([a], Int) -> [a]
untransform (ys, k) = (recreate (length ys) ys) !! k

--------------------

takeCols :: Int -> [[a]] -> [[a]]
takeCols k = map (take k)

rrot :: [a] -> [a]
rrot xs = [last xs] ++ init xs

hdsort :: Ord a => [[a]] -> [[a]]
hdsort = sortBy cmp
  where cmp (x:xs) (y:ys) = compare x y

consCol :: Ord a => ([a], [[a]]) -> [[a]]
consCol (xs, xss) = zipWith (:) xs xss

-- recreate j . map last . sort . rots == takeCols j . sort . rots

recreate :: Ord a => Int -> [a] -> [[a]]
recreate 0 = map (const [])
recreate j = hdsort . consCol . fork (id, recreate (j-1))

fork (f,h) x = (f x, h x)
