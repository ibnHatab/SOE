module UnravellingGreedyAlgorithms where

import Data.List

type Data = [Datum]
type Datum = String

data Candidate = Candidate
data Value = Value

candidates :: Data -> [Candidate]
candidates = foldr extend []

value :: Candidate -> Value
value = undefined

good :: Value -> Bool
good _ = True

solution :: Data -> [Candidate]
solution = filter (good . value) . candidates

extend :: Datum -> [Candidate] -> [Candidate]
extend = undefined

-- for all v: good v => ok v

ok :: Value -> Bool
ok _ = False

-- axiom 1,2
-- filter (good . value) = filter (good . value) . filere (ok . value)
-- `ok` value are the extention of `ok` value
-- filre (ok.value) . extend x =
--          filre (ok.value) . extend x . filre (ok.value)
-- plus fussion law of foldr

solution' :: Data -> [Candidate]
solution' = filter (good . value) . foldr extend' []

extend' :: Datum -> [Candidate] -> [Candidate]
extend' = undefined

-- remove value recalculation
-- map value . extend x = modify x . map value

candidates' :: Data -> [Candidate]
candidates' = map (fork (id, value)) . foldr extend' []

fork = undefined
{-
solution'' :: Data -> [Candidate]
solution'' = map fst . filter (good . snd) . foldr expand []

expand :: Datum -> [Candidate] -> [Candidate]
expand x =  filter (ok . snd) . zip . cross (extend x, modify x) . unzip

-}
modify= undefined

cross (f,g)(x,y) = (f x, g y)
-- fork (f . h, g . k) = cross (f,g) . fork (h,k)

-- END of THEORY
------------------------

-- shortest upravel

minBy f = foldl1 (cmp f)
cmp f u v = if f u <= f v then u else v

supravel :: Ord a => [a] -> [[a]]
supravel = minBy length . filter (all up) . unravels

unravels :: Ord a => [a] -> [[[a]]]
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
unravels = foldr (concatMap . prefixes) [[]]
prefixes :: t -> [[t]] -> [[[t]]]
prefixes x [] = [[[x]]]
prefixes x (xs:xss) =  ((x:xs) : xss) : map (xs :) (prefixes x xss)

up [] = True
up [_] = True
up (x : y : xs) = x <= y && up xs

-----------------

upravel :: Ord a => [a] -> [[[a]]]
-- upravel = filter (all up) . unravels
-- easy fission law
upravel = foldr (concatMap . uprefixes) [[]]
uprefixes x [] = [[[x]]]
uprefixes x (xs:xss) = if x <= head xs
                       then ((x:xs):xss) : map (xs:) (uprefixes x xss)
                       else (uprefixes x xss)

-- minBy length . uprefixes x ~~> insert x
heads :: Ord a => [[a]] -> [a]
heads = sort . map head

insert' :: Ord a => a -> [[a]] -> [[a]]
insert' x [] = [[x]]
insert' x (xs:xss) = if x <= head xs
                     then (x:xs) : xss
                     else xs : insert' x xss

supravel' :: Ord a => [a] -> [[a]]
supravel' = foldr insert' []
