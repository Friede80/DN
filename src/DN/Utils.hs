module DN.Utils where

import           DN.NetworkTypes

import           Data.List
import           Data.Ord

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
dot :: [Double] -> [Double] -> Double
dot x y = sum (zipWith (*) x' y')
  where
    x' = unit x
    y' = unit y

-- Normalizes a vector to unit length
unit :: [Double] -> [Double]
unit xs | all (==0) xs = xs
        | otherwise = map (/vecLen) xs
  where
    vecLen = sqrt . sum $ map (^2) xs

-- TopK Competition between neurons
topK :: Int -> [Double] -> [Double]
topK k xs = map (max 0 . normalize min' max') xs
  where
    sorted = sortBy (comparing Down) xs
    max' = head sorted
    min' = sorted!!k

updateWeights :: [Double]-> Response -> Double -> [Double]
updateWeights ws y rate = zipWith learn ws y
  where
    learn w s = (1 - rate) * w + (rate * s)
