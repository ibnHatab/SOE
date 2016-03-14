module Efficiency where

data BinTree a = Leaf a | Fork (BinTree a) (BinTree a) deriving (Show, Eq)


labels (Leaf x) = x
labels (Fork l r) = labels l ++ labels r

build [x] = Leaf x
build xs = Fork (build l) (build r)
  where (l,r) = (take m xs, drop m xs)
        m = length xs `div` 2


t = Fork
    (Fork (Leaf 'a')
     (Fork (Leaf 'b') (Leaf 'c')))
    (Fork (Leaf 'd')
     (Fork (Leaf 'e') (Leaf 'f')))

build' xs = fst (build2 (length xs) xs)

build2 :: Integral a => a -> [a1] -> (BinTree a1, [a1])
build2 1 xs = (Leaf (head xs), tail xs)
build2 n xs = ( Fork u v, xs'')
              where (u, xs') = build2 m xs
                    (v, xs'') = build2 (n-m) xs'
                    m = n `div` 2
