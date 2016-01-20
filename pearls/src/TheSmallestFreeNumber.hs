module TheSmallestFreeNumber where

import Data.List
import Data.Array.IArray

ts :: [Int]
ts = [08,23,09,00,12,11,01,10,13,07,41,04,14,21,05,17,03,19,02,06]

minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

minfree' = search . checklist
  where
    search :: Array Int Bool -> Int
    search = length . takeWhile id . elems
    checklist :: [Int] -> Array Int Bool
    checklist xs = accumArray (||) False (0, n)
                   (zip (filter (<=n) xs) (repeat True))
      where n = length xs

minfree'' xs = minfrom 0 (length xs, xs)

minfrom a (_, []) = a
minfrom a (n, xs) = if m == b-a
                    then minfrom b (n-m, qs)
                    else minfrom a (m, ps)
  where (ps, qs) = partition (<b) xs
        b = a + 1 + n `div` 2
        m = length ps

main :: IO()
main = print (minfree ts)
