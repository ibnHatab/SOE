module BuildingATreeWithMinimumHeight where

data Tree = Leaf Int | Fork (Tree) (Tree) deriving Show

tree_2 :: Tree
tree_2 = Fork
            (Fork
                (Fork (Leaf 8) (Leaf 2))
                (Leaf 7))
            (Fork
                (Leaf 9)
                (Fork (Leaf 6)
                    (Fork (Leaf 3) (Leaf 5))))

fringe :: Tree -> [Int]
fringe (Leaf x) = [x]
fringe (Fork l r) = fringe l ++ fringe r

cost :: Tree -> Int
cost (Leaf x) = x
cost (Fork l r) = 1 + max (cost l) (cost r)


trees :: [Int] -> [Tree]
trees [x] = [Leaf x]
trees (x : xs) = concatMap (prefixes x) (trees xs)

prefixes :: Int -> Tree -> [Tree]
prefixes x t@(Leaf y) = [Fork (Leaf x) t]
prefixes x t@(Fork u v) = [Fork (Leaf x) t] ++
                          [Fork u' v | u' <- prefixes x u]

foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn _ g [x] = g x
foldrn f g (x : xs) = f x (foldrn f g xs)

trees' :: [Int] -> [Tree]
trees' = foldrn (concatMap . prefixes) (wrap . Leaf)

wrap x = [x]
--------------

trees'' = map rollup . forests

type Forest = [Tree]
forests :: [Int] -> [Forest]
forests = foldrn (concatMap . prefixes') (wrap . wrap . Leaf)

prefixes' :: Int -> Forest -> [Forest]
prefixes' x ts = [Leaf x : rollup (take k ts) : drop k ts |
                  k <- [1 .. length ts]]

rollup :: Forest -> Tree
rollup = foldl1 Fork

mincostTree :: [Int] -> Tree
mincostTree = minBy cost . trees''

minBy f = foldl1 (cmp f)
cmp f u v = if f u <= f v then u else v
