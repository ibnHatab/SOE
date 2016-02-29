module TheBoyerMooreAlgorithm where

import Data.List


matches :: Eq a => [a] -> [a] -> [Int]
matches ws = map length . filter (endswith ws) . inits

endswith :: Eq a => [a] -> [a] -> Bool
endswith = isSuffixOf

-------------

-- map (foldl op e) . inits = scanl op e
-- map f . filter p = map fst . filter snd . map (fork (f,p))

fork (f,p) x = (f x, p x)

matches' :: Eq a => [a] -> [a] -> [Int]
matches' ws = map fst . filter snd . map (fork (length, endswith ws)) . inits

-- matches = foldl op e
-- fork(foldl op1 e1, foldl op2 e2) = foldl op (e1,e2), op (a,b) x = (op1 a x, op2 b x)
