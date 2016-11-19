module DN.Core (
    runNewNetwork
  , runNetwork
  ) where

import           DN.HiddenLayer
import           DN.MotorLayer
import           DN.NetworkTypes

import           Debug.Trace

runNetwork :: Network -> [[Double]] -> [[Double]] -> Network
runNetwork net zs xs = foldl stepNet net (zip zs xs)

-- Input: A list of motor inputs and a list of sensor inputs
-- Runs the DN over these inputs
runNewNetwork :: [[Double]] -> [[Double]] -> Network
runNewNetwork (z:z':zs) (x:x':xs) = runNetwork readyNet zs xs
  where
    runFirst :: Network -> Network
    runFirst (Network hidden motor) = Network (stepHidden hidden x z) motor
    runSecond :: Network -> Network
    runSecond  (Network hidden motor) = Network (stepHidden hidden x' z') motor
    readyNet = runSecond . runFirst $ initialNetwork z

stepNet :: Network -> (Response, Response) -> Network
stepNet (Network hidden motor) (z, x) =
  let newMotorLayer = if all (==0) z
                       then stepMotor motor (hOldResponse hidden)
                       else stepMotorSupervised motor (hOldResponse hidden) z
      newHiddenLayer = stepHidden hidden x (mResponse newMotorLayer)
  in Network newHiddenLayer newMotorLayer

-- The default starting Network
-- TODO Randomly generate initial weights
-- TODO Size of weight vectors is dependant on input size
initialNetwork :: [Double] -> Network
initialNetwork z = Network hidden motor
  where
    hidden = HiddenLayer { hResponse = []
                         , hOldResponse = []
                         , hNeurons = initialYNeurons }
    motor = MotorLayer { mResponse = z
                       , mNeurons = initialNeurons }
    initialYNeurons = [ YNeuron { topDownWeights = [0.3, 0.1, 0.7, 0.8]
                                , bottomUpWeights = [0.5, 0.1, 0.2]
                                , yAge = 0 }
                      , YNeuron { topDownWeights = [0.7, 0.6, 0.9, 0.9]
                                , bottomUpWeights = [0.4, 0.8, 0.1]
                                , yAge = 0 } ]
    initialNeurons = replicate 4 Neuron { weights = [0,0]
                                        , age = 0 }
