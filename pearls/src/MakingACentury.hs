module MakingACentury where

import Data.List

type Expression = [Term]
type Term = [Factor]
type Factor = [Digit]
type Digit = Int

digits :: Factor
digits = [1 .. 9]

valExpr :: Expression -> Int
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFactor

valFactor :: Factor -> Int
valFactor = foldl1 (\ n d -> 10*n + d)

good :: Int -> Bool
good = (==) 100

expressions :: [Digit] -> [Expression]
expressions = foldMap partitions . partitions

partitions :: [a] -> [[[a]]]
partitions = undefined

expressions' :: [Digit] -> [Expression]
expressions' = foldr extend []

extend :: Digit -> [Expression] -> [Expression]
extend x [] = [[[[x]]]]
extend x xs = concatMap (glue x) xs

glue :: Digit -> Expression -> [Expression]
glue x ((xs : xss) : xsss) = [ ((x : xs) : xss) : xsss,
                               ([x] : xs : xss) : xsss,
                               [[x]] : (xs : xss) : xsss ]


res = filter (good . valExpr) (expressions' digits)

ok :: Int -> Bool
ok = (<=) 100
-----------------------

value :: Expression -> (Int, Int, Int, Int)
value ((xs : xss) : xsss) = (10^n, valFactor xs, valTerm xss, valExpr xsss)
                            where n = length xs


modify :: Num t => t -> (t, t, t, t) -> [(t, t, t, t)]
modify x (k,f,t,e) =
  [(10*k, k*x+f, t, e), (10*x, f*t, t, e), (10, x, 1, f*t+e)]

good' c (k,f,t,e) = (f*t + e == c)
ok' c (k,f,t,e) = (f*t + e <= c)

solutions :: Int -> [Digit] -> [Expression]
solutions c = map fst . filter (good' c . snd) . foldr (expand c) []

expand c x [] = [([[[x]]], (10, x, 1, 0))]
expand c x evs = concat (map (filter (ok' c . snd) . glue' x) evs)

glue' x ((xs : xss) : xsss, (k, f, t, e)) =
    [(((x : xs) : xss) : xsss, (10*k, k*x + f, t, e)),
    (([x] : xs : xss) : xsss, (10, x, f * t, e)),
    ([[x]] : (xs : xss) : xsss, (10, x, 1, f * t + e))]
