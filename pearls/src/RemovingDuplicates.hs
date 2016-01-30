module RemovingDuplicates where

import Data.List hiding (insert)
import Data.Set hiding (elem, (\\))

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : (nub' (xs \\ [x]) )

-- nub = maximum . longest . filter nodups . subseq
nub'' :: Ord a => [a] -> [a]
nub'' [] = []
nub'' (x:xs) = if x `notElem` xs
               then x : nub'' xs
               else (x : nub'' (xs \\ [x])) `min` (nub'' xs)

-------------------------------------

nub''' :: Ord a => [a] -> [a]
nub''' = hub []

hub :: Ord a => [a] -> [a] -> [a]
hub ws [] = []
hub ws (x:xs) =
  case (x `elem` xs, x `elem` ws) of
  (False, False) -> us ++ [x] ++ hub [] (xs\\us)
  (False, True) -> us ++ [x] ++ hub (tail vs) (xs\\us)
  (True, False) -> hub (us ++ [x]) xs
  (True, True) -> hub ws xs
  where (us, vs) = span (< x) ws

------------------------------------
nubS :: Ord a => [a] -> [a]
nubS = hub' empty . preprocess

preprocess :: Ord a => [a] -> [(a, Set a)]
preprocess xs = zip xs (tail (scanr insert empty xs))

hub' :: Ord a => Set a -> [(a, Set a)] -> [a]
hub' ws [] = []
hub' ws ((x, xs) : xss) =
  case (x `member` xs, x `member` ws) of
  (False, False) -> eus ++ [x] ++ hub' empty yss
  (False, True) -> eus ++ [x] ++ hub' vs yss
  (True, False) -> hub' (insert x us) xss
  (True, True) -> hub' ws xss
  where (us, vs) = split x ws
        eus = elems us
        yss = [(x, xs) | (x, xs) <- xss, not (member x us)]

-------------------------------------
nubS' :: Ord a => [a] -> [a]
nubS' = hub'' empty empty . preprocess'

preprocess' :: Ord a => [a] -> [(a, Set a)]
preprocess' xs = zip xs (tail (scanr insert empty xs))

hub'' :: Ord a => Set a -> Set a -> [(a, Set a)] -> [a]
hub'' ps ws [] = []
hub'' ps ws ((x, xs) : xss) =
    if member x ps then
        hub'' ps ws xss
    else case (member x xs, member x ws) of
        (False, False)   -> eus ++ [x] ++ hub'' qs empty xss
        (False, True)    -> eus ++ [x] ++ hub'' qs vs xss
        (True, False)    -> hub'' ps (insert x us) xss
        (True, True)     -> hub'' ps ws xss
        where (us, vs) = split x ws
              eus = elems us
              qs = Prelude.foldr insert ps eus
