module DN.MotorLayer where

import           DN.NetworkTypes
import           DN.Utils

stepMotorSupervised :: MotorLayer -> Response -> Response -> MotorLayer
stepMotorSupervised m y z = hebbianLearnMotor m z y

stepMotor :: MotorLayer -> Response -> MotorLayer
stepMotor m y = hebbianLearnMotor m newResp y
  where
    newResp = motorResponse m y

hebbianLearnMotor :: MotorLayer  -> Response -> Response -> MotorLayer
hebbianLearnMotor m z y = MotorLayer
                            { mResponse = z
                            , mNeurons = zipWith updateNeuron z (mNeurons m) }
  where
    updateNeuron :: Double -> Neuron -> Neuron
    updateNeuron z' n
      | z' > 0 = Neuron { weights = updateWeights (weights n) y (learningRate n)
                           , age = age n + 1  }
      | otherwise = n

learningRate :: Neuron -> Double
learningRate n = 1 / fromIntegral (age n + 1)

motorResponse :: MotorLayer -> Response -> Response
motorResponse m y = topK 1 $ fmap (dot y . weights) (mNeurons m)
