module WhatIs where

import Data.List
import Data.Bifunctor

units, teens, tens :: [String]
units = ["zero","one","two","three","four","five", "six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourteen", "fifteen","sixteen","seventeen","eighteen", "nineteen"]
tens = ["twenty","thirty","forty","fifty","sixty", "seventy","eighty","ninety"]

-- convert :: Int -> String
convert = convert6

-- digits n = (div n 100, div (n - (div n 100) * 100 ) 10, mod n 10)

digits = unfoldr (\n -> case () of
                    _| n < 1 -> Nothing
                     | otherwise -> Just (mod n 10, div n 10))

convert2 :: Int -> String
convert2 n
  | t==0= units!!u
  | t==1= teens!!u
  | u==0= tens!!(t-2)
  | otherwise= tens!!(t-2) ++ "-" ++ units!!u
  where (t,u)= (n `div` 10, n `mod` 10)

convert3 :: Int -> String
convert3 n
  | h==0= convert2 t
  | n==0= units!!h ++ " hundred"
  | otherwise= units!!h ++ " hundred and " ++ convert2 t
  where (h,t)= (n `div` 100, n `mod` 100)


convert6 :: Int -> String
convert6 n
  | m==0= convert3 h
  | h==0= convert3 m ++ " thousand"
  | otherwise= convert3 m ++ " thousand" ++ link h ++
               convert3 h
  where (m,h)= (n `div` 1000,n `mod` 1000)

link h | h < 100 = " and "
       | otherwise = " "

------------------------------

data Tree a = Tip a | Fork (Tree a) (Tree a) deriving Show

t :: Tree Integer
t = Fork (Tip 1) (Fork (Tip 2) (Tip 3))

instance Functor Tree where
  fmap f (Tip x) = Tip (f x)
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)


disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs ys = all (==True) [x /= y | x<-xs, y<-ys]

disjoint' _ [] = True
disjoint' [] _ = True
disjoint' xs'@(x:xs) ys'@(y:ys)
  | x < y = disjoint xs ys'
  | x == y = False
  | otherwise = disjoint xs' ys

fork :: (t2 -> t, t2 -> t1) -> t2 -> (t, t1)
fork (f,g) x = (f x, g x)

cross :: (a -> t, b -> t1) -> (a, b) -> (t, t1)
cross (f,g) = fork (f . fst, g . snd)

----------------------------
