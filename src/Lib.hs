module Lib
  ( runNetwork
  ) where

import Data.List
import Data.Ord

--Configuration Constants
-- TODO: Get inside a reader monad?
k = 1
t1 = 20
t2 = 200
gamma = 2000
c = 2

type Response = [Double]
data Neuron = Neuron { weights :: [Double]
                     , age :: Int
                     } deriving Show

data Neuron2 = Neuron2 { topDownWeights :: [Double]
                       , bottomUpWeights :: [Double]
                       , age2 :: Int
                       } deriving Show

data SensorLayer = SensorLayer { input :: [Double] } deriving Show

data HiddenLayer = HiddenLayer { neurons2 :: [Neuron2] } deriving (Show)

data MotorLayer = MotorLayer { neurons :: [Neuron] } deriving Show

data Network = Network HiddenLayer MotorLayer deriving Show

initialNetwork :: Network
initialNetwork = Network hidden motor
  where
    hidden = HiddenLayer { neurons2 = initialNeurons2 }
    motor = MotorLayer { neurons = initialNeurons }
    initialNeurons2 = [ Neuron2 { topDownWeights = [0.3, 0.1, 0.7, 0.8]
                                , bottomUpWeights = [0.5, 0.1, 0.2]
                                , age2 = 0 }
                      , Neuron2 { topDownWeights = [0.7, 0.6, 0.9, 0.9]
                                , bottomUpWeights = [0.4, 0.8, 0.1]
                                , age2 = 0 } ]
    initialNeurons = replicate 4 Neuron { weights = [0,0]
                                        , age = 0 }

runNetwork :: [[Double]] -> [[Double]] -> Network
runNetwork (z:zs) (x:xs) = foldl runNetwork' (runFirst initialNetwork) (zip zs xs)
  where
    runFirst :: Network -> Network
    runFirst (Network hidden motor) = let newHiddenResponse = hiddenResponse z x hidden
                                          newHiddenLayer = hebbianLearnHidden x newHiddenResponse z hidden
                                      in  Network newHiddenLayer motor

runNetwork' :: Network -> (Response, Response) -> Network
runNetwork' (Network hidden motor) (z, x) = let newHiddenResponse = hiddenResponse z x hidden
                                            in undefined

hebbianLearnHidden :: Response -> Response -> Response -> HiddenLayer -> HiddenLayer
hebbianLearnHidden x y z hidden
  | length (neurons2 hidden) == length y = HiddenLayer {neurons2 = zipWith updateNeuron y (neurons2 hidden)}
  | otherwise = HiddenLayer {neurons2 = zipWith updateNeuron y (newNeuron2 : neurons2 hidden)}
  where
    newNeuron2 = Neuron2 { topDownWeights = z
                         , bottomUpWeights = x
                         , age2 = 0 }
    updateNeuron r n
      |  r > 0.99 = Neuron2 { topDownWeights = updateHiddenWeights (topDownWeights n) z (lRate n)
                            , bottomUpWeights = updateHiddenWeights (bottomUpWeights n) x (lRate n)
                            , age2 = age2 n + 1 }
      | otherwise = n

updateHiddenWeights :: [Double] -> [Double] -> Double -> [Double]
updateHiddenWeights w sig learningRate = zipWith learn w sig
  where
    learn w s = (1 - learningRate) * w + (learningRate * s)

lRate :: Neuron2 -> Double
lRate n = (1+mu) / a
  where
    a = fromIntegral (age2 n)
    mu | a < t1 = 0
       | a < t2 = c * (a - t1) / (t2-t1)
       | otherwise = c * (a - t2) / gamma;


hebbianLearnMotor :: Neuron -> Response -> [Double]
hebbianLearnMotor z y = zipWith learn (weights z) y
   where
     learningRate = 1 / fromIntegral (age z + 1)
     learn w y' = (1 - learningRate) * w + (learningRate * y')

topDownResponse :: Response -> HiddenLayer -> Response
topDownResponse z y = topK k $ fmap (dot z . topDownWeights) (neurons2 y)

bottumUpResponse :: Response -> HiddenLayer -> Response
bottumUpResponse x y = topK k $ fmap (dot x . bottomUpWeights) (neurons2 y)

hiddenResponse :: Response -> Response -> HiddenLayer -> Response
hiddenResponse z x y = hiddenTopK k $ zipWith (+) (topDownResponse z y) (bottumUpResponse x y)

hiddenTopK :: Int -> [Double] -> [Double]
hiddenTopK k ys = if isPerfectMatch
                    then map (max 0 . normalize min' max') ys
                    else 1: map (const 0) ys
  where
    isPerfectMatch = any (>0.99) ys
    sorted = sortBy (comparing Down) ys
    max' = head sorted
    min' = sorted!!k

motorResponse :: Response -> MotorLayer -> Response
motorResponse y z = topK k $ fmap (dot y . weights) (neurons z)

-- Take the current motor response and the hidden response to update the weights accordingly
updateMotorWeights :: Response -> Response -> MotorLayer -> MotorLayer
updateMotorWeights z y motor = MotorLayer $ zipWith updateNeuron z (neurons motor)
  where
    updateNeuron :: Double -> Neuron -> Neuron
    updateNeuron z' currentNeuron
      | z' > 0.99 = Neuron { weights = hebbianLearn currentNeuron y
                           , age = age currentNeuron + 1  }
      | otherwise = currentNeuron

hebbianLearn :: Neuron -> Response -> [Double]
hebbianLearn z y = zipWith learn (weights z) y
   where
     learningRate = 1 / fromIntegral (age z + 1)
     learn w y' = (1 - learningRate) * w + (learningRate * y')

topK :: Int -> [Double] -> [Double]
topK k xs = map (max 0 . normalize min' max') xs
  where
    sorted = sortBy (comparing Down) xs
    max' = head sorted
    min' = sorted!!k

normalize :: Double -> Double -> Double -> Double
normalize min' max' x = (x - min') / (max' - min')


dot :: (Num a) => [a] -> [a] -> a
dot x y = sum (zipWith (*) x y)
