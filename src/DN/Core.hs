module DN.Core (
    runNewNetwork
  , runNetwork
  , testNetwork
  ) where

import           DN.HiddenLayer
import           DN.MotorLayer
import           DN.NetworkTypes
import           DN.SensorLayer

import           Control.Monad
import           Control.Monad.Trans.Writer.Lazy
import           Debug.Trace

testNetwork :: Network -> [[Double]] -> [[Double]] -> [(Response,Response)]
testNetwork net zs xs = responses
  where
    responses = execWriter $ foldM stepTest net (zip testZs xs)
    testZs = bkgZ:bkgZ:repeat emptyZ
    emptyZ = fmap (const 0) bkgZ
    bkgZ = head zs

stepTest :: Network -> (Response,Response) -> Writer [(Response,Response)] Network
stepTest net (z,x) = do
  tell [(sResponse sensor, mResponse motor)]
  return newNet
  where
    newNet@(Network sensor _ motor) = stepNet True net (z,x)


runNetwork :: Network -> [[Double]] -> [[Double]] -> Network
runNetwork net zs xs = foldl (stepNet False) net (zip zs xs)

-- Input: A list of motor inputs and a list of sensor inputs
-- Runs the DN over these inputs
runNewNetwork :: [[Double]] -> [[Double]] -> Network
runNewNetwork zs xs = runNetwork readyNet zs xs
  where
    bkgX = head xs
    bkgZ = head zs
    runHidden (Network sensor hidden motor) = Network sensor (stepHidden False hidden bkgX bkgZ) motor
    readyNet = runHidden . runHidden $ initialNetwork bkgX bkgZ


stepNet :: Bool -> Network -> (Response, Response) -> Network
stepNet frzn (Network sensor hidden motor) (z, x) =
  let newSensorLayer = if all (==0) x
                         then stepSensor sensor (hOldResponse hidden)
                         else stepSensorSupervised sensor (hOldResponse hidden) x
      newMotorLayer = if all (==0) z
                       then stepMotor motor (hOldResponse hidden)
                       else stepMotorSupervised motor (hOldResponse hidden) z
      newHiddenLayer = stepHidden frzn hidden (sResponse newSensorLayer) (mResponse newMotorLayer)
  in Network newSensorLayer newHiddenLayer newMotorLayer

-- The default starting Network
-- TODO Randomly generate initial weights
initialNetwork :: [Double] -> [Double] -> Network
initialNetwork x z = Network sensor hidden motor
  where
    hidden = HiddenLayer { hResponse = []
                         , hOldResponse = []
                         , hNeurons = initialYNeurons }
    motor = MotorLayer { mResponse = z
                       , mNeurons = initialMNeurons }
    sensor = SensorLayer { sResponse = z
                         , sNeurons = initialSNeurons }
    initialYNeurons = [ YNeuron { topDownWeights = [0.3, 0.1, 0.7, 0.8]
                                , bottomUpWeights = [0.5, 0.1, 0.2]
                                , yAge = 0 }
                      , YNeuron { topDownWeights = [0.7, 0.6, 0.9, 0.9]
                                , bottomUpWeights = [0.4, 0.8, 0.1]
                                , yAge = 0 } ]
    initialSNeurons = replicate (length x) Neuron { weights = [0,0]
                                                  , age = 0 }
    initialMNeurons = replicate (length z) Neuron { weights = [0,0]
                                                  , age = 0 }
