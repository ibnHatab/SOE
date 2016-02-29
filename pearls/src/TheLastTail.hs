module TheLastTail where

import Data.List

maxtail :: Ord a => [a] -> [a]
maxtail = maximum . tails
