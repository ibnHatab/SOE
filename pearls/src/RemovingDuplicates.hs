module RemovingDuplicates where

import Data.List

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : (nub' (xs \\ [x]) )
