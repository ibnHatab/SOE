module RankingSuffixes where

import Data.List
import Data.Array

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs = xs : tails'(tail xs)

rank :: Ord a => [a] -> [Int]
rank xs = map (\x -> length (filter (<x) xs)) xs

ranktails :: Ord a => [a] -> [Int]
ranktails = rank . tails'


------------------------------------------

ranktails' :: Ord a => [a] -> [Int]
ranktails' xs = (resort n . concat . label .
               applyUntil (all single) (repartitions n) .
               psort . zip [0..]) xs
               where n = length xs

resort :: Int -> [(Int, Int)] -> [Int]
resort n = elems . array (0, n - 1)

label :: [[a]] -> [[(a, Int)]]
label iss = zipWith tag iss (scanl (+) 0 (map length iss))

tag :: [a] -> b -> [(a, b)]
tag is j = [(i, j ) | i <- is]

repartitions :: Int -> [[[Int]] -> [[Int]]]
repartitions n = map (repartition n) (iterate (* 2) 1)

repartition :: Int -> Int -> [[Int]] -> [[Int]]
repartition n k iss = concatMap (psort . map install ) iss
                      where install i = (i, if j < n then k + a!j else n - i - 1)
                                        where j = i + k
                            a = array (0, n - 1) (concat (label iss))


psort :: Ord b => [(a, b)] -> [[a]]
psort xys = pass xys []

pass [] xss = xss
pass (e@(x , y):xys) xss = step xys [] [x] [] xss
    where
        step [] as bs cs xss = pass as (bs : pass cs xss)
        step (e@(x, y'):xys) as bs cs xss
            | y' < y    = step xys (e:as) bs cs xss
            | y' == y   = step xys as (x:bs) cs xss
            | y' > y    = step xys as bs (e:cs) xss

applyUntil :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f:fs) x = if p x then x else applyUntil p fs (f x)

single :: [a] -> Bool
single [_] = True
single _ = False
