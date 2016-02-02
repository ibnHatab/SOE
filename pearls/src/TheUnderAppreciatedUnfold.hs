module TheUnderAppreciatedUnfold where


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op e [] = e
foldr' op e (x:xs) =  x `op` (foldr' op e xs)

foldr'1 f (x:xs) = foldr' f x xs -- collect

{- universal property
h = foldr op e
>> substitute
h [] = e
h (x:xs) = x `op` h xs
-}

{- fold-map
f (a `op` b) == f a `op2` f b
then
f . foldr op e = foldr op2 (f e) . map f
f . foldr1 op = foldr op2 . map f

-}

{- fold-join
h = foldr op e . map f
then
h (xs ++ ys) = h xs `op` h ys

-}

{- fold-concat
h = foldr op e . map f
then
h . concat = fold op e . map h
-}

unfold :: (b->Either () (a,b))  -> b -> [a]
unfold pfg x =
  case pfg x of
  Left () -> []
  Right (a,y) -> a : unfold pfg y

-- λ> take 5 $ unfold' (const False) (*2) (+1) 2
unfold' :: (b->Bool) -> (b->a) -> (b->b) -> b -> [a]
unfold' p f g x
  | p x = []
  | otherwise = f x : unfold' p f g (g x)

{- universal property
h = unfold' p f g
===
h x = if p x then [] else f x : h (g x)
-}

-------------------------------------------------

data Tree a = Nd a [Tree a] -- rose tree

root (Nd a ts) = a
kids (Nd a ts) = ts

type Forest a = [Tree a]

foldt :: (a->c->b) -> ([b]->c) -> Tree a -> b
foldt f g (Nd a ts) =  f a (foldf f g ts)

foldf :: (a->c->b) -> ([b]->c) -> Forest a -> c
foldf f g ts = g (map (foldt f g) ts)

sumt :: Tree Integer -> Integer
sumt = foldt (+) sum

sumf :: Forest Integer -> Integer
sumf = foldf (+) sum

t :: Tree Integer
t = Nd 1 [Nd 2 [Nd 5[], Nd 6[]],
          Nd 3 [],
          Nd 4 [Nd 7[]]]

-- λ> preordert t
-- [1,2,5,6,3,4,7]
preordert :: Tree a -> [a]
preordert = foldt (:) concat

-----------------------------------------

bftt :: Tree a -> [a]
bftt = concat . levelt

bftf :: Forest a -> [a]
bftf = concat . levelf

-- long zip with concatenate
lzc :: [[a]] -> [[a]] -> [[a]]
lzc = lzw (++)

-- long zip with
lzw :: (a->a->a) -> [a] -> [a] -> [a]
lzw op xs ys
  | null xs = ys
  | null ys = xs
  | otherwise = (head xs `op` head ys) : lzw op (tail xs) (tail ys)

glue :: [[[a]]] -> [[a]]
glue = foldr lzc []

push :: a -> [[a]] -> [[a]]
push a xss = [a] : xss

levelt :: Tree a -> [[a]]
levelt = foldt push glue

levelf :: Forest a -> [[a]]
levelf = foldf push glue

-----------------
lzw' :: (a -> a -> a) -> ([a], [a]) -> [a]
-- lzw' f = uncurry (lzw f)
lzw' op = unfold' p f g
  where
    p (xs,ys) = null xs && null ys
    f (xs,ys)
      | null xs = head ys
      | null ys = head xs
      | otherwise = head xs `op` head ys
    g (xs,ys) = (tail' xs, tail' ys)
    tail' za
      | null za = []
      | otherwise = tail za

---------- traversal as unfold -----------------------
levelf' :: Forest b -> [[b]]
levelf' = unfold' null (map root) (concat . map kids)

levelt' :: Tree b -> [[b]]
levelt' t = levelf' [t]

bftt' :: Tree a -> [a]
bftt' = concat . levelt'

bftf' :: Forest a -> [a]
bftf' = concat . levelf'

--------------deforestation------------------------
-- hilomorphism h = foldr op e . unfold p f g
bftf'' ts
  | null ts = []
  | otherwise = map root ts ++ bftf'' (concat (map kids ts))

bftt'' :: Tree t -> [t]
bftt'' ts = bftf'' [ts]
