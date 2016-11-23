module DN.SensorLayer where

import           DN.NetworkTypes
import           DN.Utils

import           Debug.Trace

stepSensorSupervised :: SensorLayer -> Response -> Response -> SensorLayer
stepSensorSupervised m y x
  | length y == neuronWidth m = hebbianLearnSensor m x y
  | otherwise = hebbianLearnSensor (addNeuronConn m) x y

addNeuronConn :: SensorLayer -> SensorLayer
addNeuronConn m = SensorLayer { sResponse = (sResponse m)
                              , sNeurons = fmap f (sNeurons m) }
  where
    f :: Neuron -> Neuron
    f n = Neuron { weights = 0:(weights n)
                 , age = age n }

stepSensor :: SensorLayer -> Response -> SensorLayer
stepSensor m y = hebbianLearnSensor m newResp y
  where
    newResp = sensorResponse m y

hebbianLearnSensor :: SensorLayer  -> Response -> Response -> SensorLayer
hebbianLearnSensor m x y = SensorLayer
                            { sResponse = x
                            , sNeurons = zipWith updateNeuron x (sNeurons m) }
  where
    updateNeuron :: Double -> Neuron -> Neuron
    updateNeuron x' n
      | x' > 0 = Neuron { weights = updateWeights (weights n) y (learningRate n)
                           , age = age n + 1  }
      | otherwise = n

learningRate :: Neuron -> Double
learningRate n = 1 / fromIntegral (age n + 1)

sensorResponse :: SensorLayer -> Response -> Response
sensorResponse m y = topK 1 $ fmap (dot y . weights) (sNeurons m)

neuronWidth :: SensorLayer -> Int
neuronWidth SensorLayer { sNeurons = ns } = length $ weights (head ns)
