module ASurpassingProblem where

-- maximum surpassing count
msc :: Ord a => [a] -> Int
msc xs =  maximum [scount z zs | z : zs <- tails xs]

scount x xs = length (filter (> x) xs)

tails [] = []
tails (x : xs) = xs : (tails xs)

ts = "GENERATING"

table xs = [(z, scount z zs) | z : zs <- tails xs]


table' [x] = [(x, 0)]
table' xs = join (m-n) (table' ys) (table' zs)
            where
              m = length xs
              n = m `div` 2
              (ys, zs) = splitAt n xs

join 0 txs [] = txs
join n [] tys = tys
join n txs@((x,c) : txs') tys@((y,d) : tys')
  | x < y  = (x, c+n) : join n txs' tys
  | x >= y = (y, d)   : join (n-1) txs tys'

msc' xs = maximum . (map snd) $ (table' xs)
