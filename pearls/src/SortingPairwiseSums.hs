module SortingPairwiseSums where

import Data.List
import Data.List.Ordered
import Data.Array
import System.Random

-- cmp :: Ord a => Num a => a -> a -> Ordering
-- cmp = compare

-- sortsum :: [Int] -> [Int] -> [Int]
-- sortsum xs ys = sortBy cmp [ x + y | x <-xs, y <-ys]

-- main1 = sortsum [7, 3, 1, 9, 5] [40, 20, 30, 60, 50]

isSorted xs = (sort xs) == xs

subs :: Num a => [a] -> [a] -> [(a, (Int, Int))]
subs xs ys = [(x-y, (i,j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]
cmp a (_, (i, j)) (_, (k, l)) = compare (a ! (1, i, k)) (a ! (2, j, l))

sortsubs :: Ord a =>  Num a => [a] -> [a] -> [(a, (Int, Int))]
sortsubs xs ys = sort (subs xs ys)

sortsubs'' :: Ord a => Num a => [a] -> [a] -> [(a, (Int, Int))]
sortsubs'' xs ys = sortBy (cmp (mkArray xs ys)) (subs xs ys)

sortsum' xs ys = map fst (sortsubs'' xs (map negate ys))

---------------------

table :: Ord a => Num a => [a] -> [a] -> [(Int, Int, Int)]
table xs ys = map snd (map (tag 1) xxs `merge` map (tag 2) yys)
              where xxs = sortsubs' xs
                    yys = sortsubs' ys

tag :: Int -> (a, (Int, Int)) -> (a, (Int, Int, Int))
tag i (x,(j,k)) = (x,(i,j,k))


mkArray :: Ord a => Num a => [a] -> [a] -> Array(Int, Int, Int) Integer
mkArray xs ys = array b (zip (table xs ys) [1..])
                where b = ((1, 1, 1), (2, p, p))
                      p = max (length xs) (length ys)

sortsubs' :: Ord a => Num a => [a] -> [(a, (Int, Int))]
sortsubs' []    = []
sortsubs' [w]   = [(w - w, (1, 1))]
sortsubs' ws = foldr1 merge [xxs, map (incr m) xys, map (incl m) yxs, map (incb m) yys]
    where xxs               = sortsubs' xs
          xys               = sortBy (cmp (mkArray xs ys )) (subs xs ys )
          yxs               = map switch (reverse xys )
          yys               = sortsubs' ys
          (xs, ys)          = splitAt m ws
          m                 = (length ws) `div` 2
          incl m (x,(i,j))  = (x, (m + i, j))
          incr m (x,(i,j))  = (x, (i, m + j))
          incb m (x,(i,j))  = (x, (m + i, m + j))
          switch (x,(i,j))  = (negate x, (j, i))
