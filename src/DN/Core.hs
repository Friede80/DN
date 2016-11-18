module DN.Core (
    runNetwork
  ) where

import DN.NetworkTypes
import DN.HiddenLayer
import DN.MotorLayer

-- Input: A list of motor inputs and a list of sensor inputs
-- Runs the DN over these inputs
runNetwork :: [[Double]] -> [[Double]] -> Network
runNetwork (z:zs) (x:xs) = foldl runNetwork' (runFirst initialNetwork) (zip zs xs)
  where
    runFirst :: Network -> Network
    runFirst (Network hidden motor) = let newHiddenResponse = hiddenResponse z x hidden
                                          newHiddenLayer = hebbianLearnHidden x newHiddenResponse z hidden
                                      in  Network newHiddenLayer motor

runNetwork' :: Network -> (Response, Response) -> Network
runNetwork' (Network hidden motor) (z, x) = let newHiddenResponse = hiddenResponse z x hidden
                                                newHiddenLayer = hebbianLearnHidden x newHiddenResponse z hidden
                                                newMotorLayer = updateMotorWeights z newHiddenResponse motor
                                            in Network newHiddenLayer newMotorLayer


-- The default starting Network
-- TODO Randomly generate initial weights
-- TODO Size of weight vectors is dependant on input size
initialNetwork :: Network
initialNetwork = Network hidden motor
  where
    hidden = HiddenLayer initialNeurons2
    motor = MotorLayer initialNeurons
    initialNeurons2 = [ Neuron2 { topDownWeights = [0.3, 0.1, 0.7, 0.8]
                                , bottomUpWeights = [0.5, 0.1, 0.2]
                                , age2 = 0 }
                      , Neuron2 { topDownWeights = [0.7, 0.6, 0.9, 0.9]
                                , bottomUpWeights = [0.4, 0.8, 0.1]
                                , age2 = 0 } ]
    initialNeurons = replicate 4 Neuron { weights = [0,0]
                                        , age = 0 }
