module DN.Core (
    runNetwork
  ) where

import           DN.HiddenLayer
import           DN.MotorLayer
import           DN.NetworkTypes

import Debug.Trace

-- Input: A list of motor inputs and a list of sensor inputs
-- Runs the DN over these inputs
runNetwork :: [[Double]] -> [[Double]] -> Network
runNetwork (z:zs) (x:xs) = foldl stepNet (runFirst initialNetwork) (zip zs xs)
  where
    runFirst :: Network -> Network
    runFirst (Network hidden motor) = Network (stepHidden hidden x z) motor

stepNet :: Network -> (Response, Response) -> Network
stepNet (Network hidden motor) (z, x) =
  let newHiddenLayer = stepHidden hidden x z
      newMotorLayer = if all (==0) z
                       then stepMotor motor (hResponse hidden)
                       else stepMotorSupervised motor (hResponse hidden) z
  in traceShow ("Motor Response", (mResponse newMotorLayer)) $ Network newHiddenLayer newMotorLayer

-- The default starting Network
-- TODO Randomly generate initial weights
-- TODO Size of weight vectors is dependant on input size
initialNetwork :: Network
initialNetwork = Network hidden motor
  where
    hidden = HiddenLayer { hResponse = []
                         , hNeurons = initialYNeurons }
    motor = MotorLayer { mResponse = []
                       , mNeurons = initialNeurons }
    initialYNeurons = [ YNeuron { topDownWeights = [0.3, 0.1, 0.7, 0.8]
                                , bottomUpWeights = [0.5, 0.1, 0.2]
                                , yAge = 0 }
                      , YNeuron { topDownWeights = [0.7, 0.6, 0.9, 0.9]
                                , bottomUpWeights = [0.4, 0.8, 0.1]
                                , yAge = 0 } ]
    initialNeurons = replicate 4 Neuron { weights = [0,0]
                                        , age = 0 }
