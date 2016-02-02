module APointlessDerivationOfRadixSort where
-- courtesy of Jeremy Gibbson

ordered :: Ord b => [a->b] -> a -> a -> Bool
ordered [] x y = True
ordered (d:ds) x y = d x <= d y && ordered ds x y

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq, Ord)

-- partition the list into buckets accordingly to most significant field
mktree :: (Bounded b, Enum b, Eq b) => [a->b] -> [a] -> Tree [a]
-- mktree [] xs = Leaf xs
-- mktree (d:ds) xs = Node (map (mktree ds) (ptn d xs))

-- mktree []  = Leaf
-- mktree (d:ds) = Node . map (mktree ds) . ptn d

mktree = foldr mf Leaf
  where mf d g = Node . map g . ptn d

-- partition the list `xs` into buckets accordingly to the field extracted bu `d`
ptn ::  (Bounded b, Enum b, Eq b) => (a->b) -> [a] -> [[a]]
ptn d xs = [filter ((== m) . d) xs | m <-rng]

rng :: (Bounded a, Enum a) => [a]
rng = [minBound..maxBound]

flatten :: Tree [a] -> [a]
flatten = foldt id concat

foldt :: (a->b) -> ([b]->b) -> Tree a -> b
foldt f g (Leaf x) = f x
foldt f g (Node ts) = g (map (foldt f g) ts)

treesort :: (Bounded b, Enum b, Eq b) => [(a->b)] -> [a] -> [a]
-- treesort ds xs = flatten (mktree ds xs)
-- treesort ds = flatten . mktree ds
treesort = comp flatten . mktree

comp :: (b->c) -> (a->b) -> (a->c)
comp f g = f .g

treesort' :: (Bounded b, Enum b, Eq b) => [(a->b)] -> [a] -> [a]
treesort' = foldr ft et
  where et = comp flatten Leaf -- id
        ft d g = concat . ptn d . g

radixsort :: (Bounded b, Enum b, Eq b) => [a->b] -> [a] -> [a]
radixsort = foldr ft id
  where ft d g = concat . ptn d . g

----------------------------------

data PitchClass = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
                deriving (Eq, Ord, Show, Read, Enum)

instance Bounded PitchClass where
  minBound = A
  maxBound = Ab

-- λ> treesort [fst, snd] [(D,E), (A,B), (C,D)]
-- [(A,B),(C,D),(D,E)]

----------------------------------
