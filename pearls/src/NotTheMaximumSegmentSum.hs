module NotTheMaximumSegmentSum where

mnss :: [Int] -> Int
mnss = maximum . map sum . nonsegs

nonsegs :: [Int] -> [[Int]]
nonsegs =  extract . filter nonseg . markings

extract :: [[(a, Bool)]] -> [[a]]
extract = map (map fst . filter snd)

markings :: [Int] -> [[(Int, Bool)]]
markings xs = [zip xs bs | bs <- booleans (length xs) ]

booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = [ b : bs | b <- [True, False], bs <- booleans (n-1) ]

-- F*T+F+T(T+F)*
-- Empty | Suffix | Middle | Non-segment
data State = E | S | M | N deriving Eq

nonseg :: [(Int, Bool)] -> Bool
nonseg = (== N) . foldl step E . map snd

step :: State -> Bool -> State
step E False = E
step E True  = S

step S True  = S
step S False = M

step M False = M
step M True  = N

step N False = N
step N True  = N

-------------------------------------------------------
mnss' :: [Int] -> Int
mnss' xs = fourth (foldl h (start (take 3 xs)) (drop 3 xs))

h :: (Num a,  Ord a) => (a, a, a, a) -> a -> (a, a, a, a)
h (e, s, m, n) x = (e, max s e + x, max m s, max n (max n m + x))

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

start :: (Num a,  Ord a) => [a] -> (a, a, a, a)
start xs = (0, maximum [x + y + z, y + z, z], maximum [x, x + y, y], x + z)
    where x = xs!!0
          y = xs!!1
          z = xs!!2
