module ImprovingOnSaddlebackSearch where

invert :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a,a)]
invert f z = [(x, y) | x <-[0..z], y <- [0..z], f x y == z ]

ft = (+)
f2 x y = 3*x+27*y+y*y

invert_b :: (Enum a, Eq a, Num a) => (a -> a -> a) -> a -> [(a,a)]
invert_b f z = [(x, y) | x <-[0..z], y <- [0..z-x], f x y == z ]
