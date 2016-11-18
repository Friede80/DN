module DN.MotorLayer where

import DN.NetworkTypes
import DN.Utils

import Data.List
import Data.Ord

motorResponse :: Response -> MotorLayer -> Response
motorResponse y (MotorLayer z) = topK 1 $ fmap (dot y . weights) z

-- Take the current motor response and the hidden response to update the weights accordingly
updateMotorWeights :: Response -> Response -> MotorLayer -> MotorLayer
updateMotorWeights z y (MotorLayer motor) = MotorLayer $ zipWith updateNeuron z motor
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

hebbianLearnMotor :: Neuron -> Response -> [Double]
hebbianLearnMotor z y = zipWith learn (weights z) y
   where
     learningRate = 1 / fromIntegral (age z + 1)
     learn w y' = (1 - learningRate) * w + (learningRate * y')
