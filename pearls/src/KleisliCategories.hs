{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module KleisliCategories where

import Data.Char
import Control.Comonad

type Writer a = (a, String)

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2  = \x ->
  let (x1, s1) = m1 x
      (x2, s2) = m2 x1
  in (x2, s1 ++ ">>" ++ s2)

return :: a -> Writer a
return x = (x, "")

upCase :: String -> Writer String
upCase s = (map toUpper s, "upcase")

toWords :: String -> Writer [String]
toWords s = (words s, "words")

process :: String -> Writer [String]
process = upCase >=> toWords

data Maybe' a = Nothing' | Just' a
  deriving (Functor, Show)

-- Bifunctor
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Functor, Show)

---------------------------------------

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x

data Stream x = Cons x (Stream x) deriving Show

instance Representable Stream where
    type Rep Stream = Integer
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) n = if n == 0 then b else index bs (n - 1)

-------------------------------------------------
-- F-Algebra
--
data ExprF a = Const Int
             | Add a a
             | Mul a a deriving Show

newtype Fix f = In (f (Fix f))

type Expr = Fix ExprF

val :: Fix ExprF
val = In (Const 12)

testExpr :: Fix ExprF
testExpr = In $ (In $ (In $ Const 2) `Add`
                (In $ Const 3)) `Mul` (In $ Const 4)

instance Functor ExprF where
    fmap eval (Const i) = Const i
    fmap eval (left `Add` right) = (eval left) `Add` (eval right)
    fmap eval (left `Mul` right) = (eval left) `Mul` (eval right)

alg :: ExprF Int -> Int
alg (Const i)   = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

alg' :: ExprF String -> String
alg' (Const i)   = [chr (ord 'a' + i)]
alg' (x `Add` y) = x ++ y
alg' (x `Mul` y) = concat [[a, b] | a <- x, b <- y]

type Algebra f a = f a -> a
type SimpleA = Algebra ExprF Int

alg'' :: SimpleA
alg'' (Const i)   = i
alg'' (x `Add` y) = x + y
alg'' (x `Mul` y) = x * y

type ExprInitAlg = Algebra ExprF (Fix ExprF)

ex_init_alg :: ExprF (Fix ExprF) -> Fix ExprF
ex_init_alg = In

unFix :: Fix f -> f (Fix f)
unFix (In x) = x

-- g testExpr
g = alg . (fmap g) . unFix

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

evalS :: Fix ExprF -> String
evalS = cata alg'

evalI :: Fix ExprF -> Int
evalI = cata alg''


-- Functor as container ---------------------------------
newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

newtype Reader e a = Reader (e -> a)

instance Functor (Reader e) where
    fmap f (Reader g) = Reader (\x -> f (g x))

runReader :: Reader e a -> e -> a
runReader (Reader f) = f

-- Yoneda lema -----------------------------------------------------------
{-# LANGUAGE ExplicitForAll #-}

imager :: forall r . ((Bool -> r) -> [r])
imager iffie = fmap iffie [True, False, True, True]

data Color = Red | Green | Blue        deriving Show
data Note  = C | D | E | F | G | A | B deriving Show

colorMap x = if x then Blue else Red
heatMap  x = if x then 32   else 212
soundMap x = if x then C    else G

main1 = print $ imager colorMap

-- Lenses -------------------------------------------------------
data Store a s = Store a (a->s)

fstl :: (a,b) -> Store a (a,b)
fstl (x, y) = Store x (\x' -> (x', y))

instance Functor (Store a) where
  fmap f (Store x h) = Store x (f . h)

instance Comonad (Store a) where
  extract (Store x h) = h x
  duplicate (Store x h) = Store x (\y -> Store y h)

type Coalgebra w s = s -> w s

get (x, y) = x
set (x, y) x' = (x', y)

coalg :: (a, b) -> Store a (a, b)
coalg s = Store (get s) (set s)

data IStore a b t = IStore a (b -> t)


class IxFunctor f where
    imap :: (s -> t) -> f a b s -> f a b t

instance IxFunctor IStore where
    -- imap :: (s -> t) -> IStore a b s -> IStore a b t
    imap f (IStore x h) = IStore x (f . h)

class IxComonad w where
    iextract :: w a a t -> t
    iduplicate :: w a b t -> w a j (w j b t)

instance IxComonad IStore where
    -- iextract :: IStore a a t -> t
    iextract (IStore a h) = h a
    -- iduplicate :: IStore a b t -> IStore a c (IStore c b t)
    iduplicate (IStore a h) = IStore a (\c -> IStore c h)
--------------------------------------------------
newtype Calc a = Calc [a]


instance Functor Calc where
  fmap f (Calc (x:xs)) = Calc $ f x : fmap f xs

const' :: t1 -> t -> t1
const' x y = x

foo :: _a -> Bool
foo _ = False
