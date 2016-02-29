{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module CategoricalProgramming where


import Criterion (bench, nf, bgroup, whnf)
import Criterion.Main (defaultMain)
import Data.List (subsequences)
import Control.DeepSeq (deepseq)


map' :: Foldable t => (a -> a1) -> t a -> [a1]
map' f = foldr (\x xs -> f x : xs) []

-- Fussion
-- foldr (:) [ ] = id
-- h (f a b) = g a (h b) ⇒
--       h ◦ foldr f b = foldr g (h b)
--

main1 = defaultMain (map createBenchmark [0, 2 .. 24])
    where
        createBenchmark n =
            let
                xs = replicate n 'x'
            in
                xs `deepseq` (bench (show n) $ nf subsequences xs)


fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "5"  $ whnf fib 5
               , bench "10" $ whnf fib 10
               , bench "15" $ whnf fib 15
               , bench "20" $ whnf fib 20
               , bench "25" $ whnf fib 25
               ]
  ]

-- Initial algebra and Catamorphism
newtype Mu f = In (f (Mu f))

unIn :: Mu f -> f (Mu f)
unIn (In x) = x

-- combinator
cata :: Functor f => (f c -> c) -> (Mu f -> c)
cata phi = phi . fmap (cata phi) . unIn


-- ex Nat
data N x = Z | S x

instance Functor N where
  fmap f Z = Z
  fmap f (S x) = S (f x)

type Nat = Mu N

natToInt :: Nat -> Int
natToInt = cata phi
  where phi Z = 0
        phi (S n) = n + 1

instance Show Nat where
  show = show . natToInt

zeroN :: Nat
zeroN = In Z

succN :: Nat -> Nat
succN n = In (S n)

oneN = succN zeroN
twoN = succN oneN
threeN = succN twoN

addN :: Nat -> Nat -> Nat
addN x y = cata phi x
  where phi Z = y
        phi (S n) = succN n

mulN :: Nat -> Nat -> Nat
mulN x y = cata phi x
  where phi :: N Nat -> Nat
        phi Z = y
        phi (S n) = addN y n

-- ex List
data L a x = N | C a x

instance Functor (L a) where
  fmap f N = N
  fmap f (C x xs) = C x (f xs)

type List a = Mu (L a)

nilL = In N

consL x xs = In (C x xs)

lengthL :: Mu (L t) -> Nat
lengthL = cata phi
  where phi N = zeroN
        phi (C _ n) = succN n


concatL :: Mu (L a) -> Mu (L a) -> Mu (L a)
concatL xs ys = cata phi xs
  where phi N = ys
        phi (C x xs') = consL x xs'

mapL :: (t -> a) -> Mu (L t) -> Mu (L a)
mapL f = cata phi
  where phi N = nilL
        phi (C x xs) = consL (f x) xs

-- Terminal coalgebra and Anamorphism
newtype Nu f = Wrap (f (Nu f))

out :: Nu t -> t (Nu t)
out (Wrap x) = x

ana :: Functor f => (c -> f c) -> (c -> Nu f)
ana phi = Wrap . fmap (ana phi) . phi

-- ex Stream

data St a x = St a x

instance Functor (St a) where
  fmap f (St x xs) = St x (f xs)

type Stream a = Nu (St a)

headS xs = case out xs of
  St x _ -> x

tailS :: Nu (St t) -> Nu (St t)
tailS xs = case out xs of
  St _ xs -> xs

zipS :: (Nu (St t), Nu (St t1)) -> Nu (St (t, t1))
zipS = ana phi
  where phi (xs, ys) = St (headS xs, headS ys) (tailS xs, tailS ys)

iterateS :: (a -> a) -> a -> Nu (St a)
iterateS f = ana phi
  where phi x = St x (f x)

-- ex CoList

type CoList a = Nu (L a)

nullCl :: Nu (L t) -> Bool
nullCl xs = case out xs of
  N -> True
  C _ _ -> False

headCl :: Nu (L t) -> t
headCl xs = case out xs of
  C x _ -> x

tailCl :: Nu (L t) -> Nu (L t)
tailCl xs = case out xs of
  C _ xs'  -> xs'

-- Paramotphism

para :: Functor f => (f (a, Mu f) -> a) -> Mu f -> a
para phi = fst . cata (fork phi (In . fmap snd))

para' :: Functor f => (f (b, Mu f) -> b) -> Mu f -> b
para' phi = phi . fmap (fork (para phi) id) . unIn

fork :: (a->b) -> (a->c) -> a -> (b, c)
fork f g x = (f x, g x)

fact :: Nat -> Nat
fact = para phi
  where phi Z = succN zeroN
        phi (S (r,x)) = mulN (succN x) r

dropWhileL :: (a -> Bool) -> Mu (L a) -> Mu (L a)
dropWhileL p = para phi
  where phi N = nilL
        phi (C x (r, xs)) | p x = r
                          | otherwise = consL x xs

-- Apomorphism

data Sum a b = InL a | InR b

join :: (a -> c) -> (b -> c) -> Sum a b -> c
join f _ (InL x) = f x
join _ g (InR x) = g x

apo :: Functor f => (c -> f (Sum c (Nu f))) -> c -> Nu f
apo phi = ana (join phi (fmap InR . out)) . InL

apo' :: Functor f => (a -> f (Sum a (Nu f))) -> a -> Nu f
apo' phi = Wrap . fmap (join (apo phi) id) . phi

insertS :: Ord a => a -> Stream a -> Stream a
insertS a = apo phi
  where phi xs | x < a = St x (InL (tailS xs))
               | otherwise = St a (InR xs)
          where x = headS xs

appendS :: (CoList a, CoList a) -> CoList a
appendS = apo phi
  where phi (xs, ys)
          | nullCl xs && nullCl ys = N
          | nullCl xs = C (headCl ys)
                        (InR (tailCl ys))
          | otherwise = C (headCl xs)
                        (InL (tailCl xs, ys))

-- Histomorphism

newtype ProdF f a x = ProdF (a, f x)

instance Functor f => Functor (ProdF f a) where
  fmap f (ProdF (a, fx)) = ProdF (a, fmap f fx)

forkF :: (a1 -> a) -> (a1 -> f x) -> a1 -> ProdF f a x
forkF f g  = ProdF . fork f g

hdCV xs = case out xs of
  ProdF (c, _) -> c

tlCV xs = case out xs of
  ProdF (_, fx) -> fx

histo :: Functor f => (f (Nu (ProdF f c)) -> c)
         -> Mu f -> c
histo phi =  phi
             . fmap (ana (forkF (histo phi) unIn))
             . unIn

fibo :: Nat  -> Int
fibo = histo phi
       where phi Z = 1
             phi (S x) =  case tlCV x of
               Z -> 1
               S y -> hdCV x + hdCV y

evens :: Mu (L a) -> Mu (L a)
evens = histo phi
       where phi N = nilL
             phi (C _ x) = case tlCV x of
               N -> nilL
               C a y -> consL a (hdCV y)

-- Futomorphism

newtype SumF f a x = SumF (Sum a (f x))

instance Functor f => Functor (SumF f a) where
  fmap f (SumF (InL a)) = SumF (InL a)
  fmap f (SumF (InR x)) = SumF (InR (fmap f x))

joinF :: (a -> c) -> (t t1 -> c) -> SumF t a t1 -> c
joinF f g (SumF s) = join f g s

lastF :: a -> Mu (SumF f a)
lastF x = In (SumF (InL x))

consF :: f (Mu (SumF f a)) -> Mu (SumF f a)
consF x = In (SumF (InR x))

futu :: Functor f => (c -> f (Mu (SumF f c)))
        -> c -> Nu f
futu phi =  ana (joinF phi id . unIn) . lastF

futu' :: Functor f => (a -> f (Mu (SumF f a))) -> a -> Nu f
futu' phi = Wrap
           . fmap (cata (joinF (futu phi) Wrap))
           . phi

exch :: Nu (St t) -> Nu (St t)
exch = futu phi
       where phi xs = St (headS (tailS xs))
                      (consF (St (headS xs)
                              (lastF (tailS xs))))
