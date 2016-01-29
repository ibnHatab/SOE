module Main where

import Data.List

data Person = Person String |
              Celebrity String
              deriving (Show, Eq)

type Party = [Person]

knows :: Person -> Person -> Bool
knows (Person p) (Celebrity c) = True
knows (Celebrity c) (Person p) = False
knows (Celebrity c1) (Celebrity c2) = True

-- Most people at the party don't know each other
-- except for Ernie and Bert
knows (Person "Ernie") (Person "Bert") = True
knows (Person "Bert") (Person "Ernie") = True
knows (Person p1) (Person p2)
    | p1 == p2  = True      -- Everyone knows themselves
    | otherwise = False     -- but no-one else.

tom = Celebrity "Tom Cruise"
cam = Celebrity "Cameron Diaz"
matt = Celebrity "Matt Damon"

john = Person "John Doe"
jane = Person "Jane Doe"
joe = Person "Joe Bloggs"
ernie = Person "Ernie"
bert = Person "Bert"

party = [bert, cam, ernie, joe, john, jane, matt, tom]
sparty = [cam, tom, joe, john]
tparty = [tom, john]

-------------------------------------

-- cclique :: Party -> [Person]
cclique ps = head (filter (cap ps) (subsequs ps))

cap :: [Person] -> [Person] -> Bool
[] `cap` _ = False
_ `cap` [] = False
cap cs ps = and [p `knows` c |  p <- ps, c <- cs]
            &&
            and [not (c `knows` p) | p <- ps, c <- cs]

subsequs [] = [[]]
subsequs (x:xs) = map (x:) (subsequs xs) ++ subsequs xs

-------------------------------------
splitIntoTwo :: [a] -> [([a], [a])]
splitIntoTwo [] = []
splitIntoTwo xs = zip subs $ reverse subs
  where subs = subsequences xs

is_clique :: ([Person], [Person]) -> Bool
is_clique ([], _)   = False
is_clique (_, [])   = False
is_clique (cs, ps)  = and [p `knows` c | c <- cs, p <- ps]
                      &&
                      and [not (c `knows` p) | c <- cs, p <- ps]

find_clique :: Party -> [Person]
find_clique p = head [cs | (cs, ps) <- splitIntoTwo p, is_clique (cs, ps)]

----------------------------------------
nonmember :: Person -> [Person] -> Bool
nonmember p cs = and [p `knows` c && not (c `knows` p) | c <- cs]

iff :: Bool -> Bool -> Bool
iff a b = (not a || b) && (not b || a)

member :: Person -> [Person] -> [Person] -> Bool
member p ps cs = and [x `knows` p && iff (p `knows` x) (x `elem` cs) | x <- ps]

cclique' :: Party -> [Person]
cclique' = head . ccliques

ccliques [] = [[]]
ccliques (p : ps) = map (p:) (filter (member p ps) css) ++
                    filter (nonmember p) css
                    where css = ccliques ps

------------------------------------

cclique'' :: Party -> [Person]
cclique'' = foldr op []
op p cs
    | null cs           = [p]
    | not (p `knows` c) = [p]
    | not (c `knows` p) = cs
    | otherwise         = p:cs
    where c = head cs

-------------------------------------
main = putStrLn $ show $ cclique' party
