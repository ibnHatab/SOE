module ASelectionProblem where

import Data.Array

smallest k (xs, us) = union (xs, us) !! k

union (xs, []) = xs
union ([], ys) = ys
union (xss@(x:xs), yss@(y:ys))
  | x < y = x : union (xs, yss)
  | x >= y = y : union (xss, ys)


main = show $ smallest 3 ([1, 2, 3, 9], [4, 10, 13, 33, 67])


smallest' k (zs, []) = zs !! k
smallest' k ([], ws) = ws !! k
smallest' k (zs, ws) =
  case (a < b, k <= p+q) of
  (True, True) -> smallest' k (zs, us)
  (True, False) -> smallest' (k-p-1) (ys, ws)
  (False, True) -> smallest' k (xs, ws)
  (False, False) -> smallest' (k-q-1) (zs, vs)
  where
    p = (length zs) `div` 2
    q = (length ws) `div` 2
    (xs, a : ys) = splitAt p zs
    (us, b : vs) = splitAt q ws

smallest'' :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest'' k (xa, ya) = search k (0, m+1) (0, n+1)
  where (0, m) = bounds xa
        (0, n) = bounds ya
        search k (lx, rx)(ly, ry)
          | lx == rx = xa ! k
          | ly == ry = ya ! k
          | otherwise = case (xa!mx < ya!my, k <= mx + my) of
            (True, True) -> search k (lx, rx) (ly, my)
            (True, False) -> search (k-mx-1) (mx, rx) (ly, ry)
            (False, True) -> search k (lx, mx) (ly, ry)
            (False, False) -> search (k-my-1) (lx, rx) (my, ry)
          where mx = (lx+rx) `div` 2
                my = (ly+ry) `div` 2

main' = smallest'' 3 (
  listArray (0, length xs) xs,
  listArray (0, length ys) ys)
  where (xs, ys) = ([1, 2, 3, 9], [4, 10, 13, 33, 67])
