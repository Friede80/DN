module DN.MotorLayer where

import           DN.NetworkTypes
import           DN.Utils

import           Debug.Trace

stepMotorSupervised :: MotorLayer -> Response -> Response -> MotorLayer
stepMotorSupervised m y z
  | length y == neuronWidth m = hebbianLearnMotor m z y
  | otherwise = hebbianLearnMotor (addNeuronConn m) z y

addNeuronConn :: MotorLayer -> MotorLayer
addNeuronConn m = MotorLayer { mResponse = (mResponse m)
                             , mNeurons = fmap f (mNeurons m) }
  where
    f :: Neuron -> Neuron
    f n = Neuron { weights = 0:(weights n)
                 , age = age n }

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

neuronWidth :: MotorLayer -> Int
neuronWidth MotorLayer { mNeurons = ns } = length $ weights (head ns)
