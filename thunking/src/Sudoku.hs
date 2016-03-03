module Sudoku where

type Matrix a = [Row a]
type Row a = [a]

type Grid = Matrix Digit
type Digit = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

----------------------------------

solve :: Grid -> [Grid]
solve = filter valid . completions

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)
  where
    nodups :: Eq a => [a] -> Bool
    nodups [] = True
    nodups (x:xs) = x `notElem` xs && nodups xs
    rows = id
    cols [xs] = [[x] | x <- xs]
    cols (xs:xss) = zipWith (:) xs (cols xss)
    boxs = map ungroup . ungroup . map cols . group . map group
      where ungroup = concat
            group [] = []
            group xs = take 3 xs : group (drop 3 xs)

completions :: Grid -> [Grid]
completions = expand . prune . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice)
  where
    choice :: Digit -> [Digit]
    choice d = if blank d then digits else [d]

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp
  where
    cp :: [[a]] -> [[a]]
    cp [] = [[]]
    cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
               where yss = cp xss

prune :: Matrix [Digit] -> Matrix [Digit]
prune = undefined
  where pruneRow row = map (remove fixed) row
          where fixed = [d | [d] <- row]
                remove ds [x] = [x]
                remove ds xs = filter (`notElem` ds) xs

{--
4..!96.!37.
...!321!...
5..!87.!261
---!---!---
.8.!.9.!...
.92!.4.!5.6
..4!5.8!.9.
---!---!---
9.3!.57!..4
248!1.9!...
.5.!482!1..
-}
s = [
  "400960370",
  "000321000",
  "500870261",
  "080090000",
  "092040506",
  "004508090",
  "903057004",
  "248109000",
  "050482100"
  ]
