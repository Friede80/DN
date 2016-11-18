module DN.Utils where

--Configuration Constants
-- TODO: Get inside a reader monad?
t1, t2, gamma, c :: Double
t1 = 20
t2 = 200
gamma = 2000
c = 2

-- Scales x from 0-1 proportionally to min' and max'
normalize :: Double -> Double -> Double -> Double
normalize min' max' x = (x - min') / (max' - min')

-- The dot product of two vectors
dot :: (Num a) => [a] -> [a] -> a
dot x y = sum (zipWith (*) x y)
