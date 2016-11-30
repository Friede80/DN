module DN.ExternalLayer where

import           DN.NetworkTypes
import           DN.Utils

import           Debug.Trace

stepLayerSupervised :: ExternalLayer -> Response -> Response -> ExternalLayer
stepLayerSupervised l y r
  | length y == neuronWidth l = hebbianLearnLayer l r y
  | otherwise = hebbianLearnLayer (addNeuronConn l) r y

addNeuronConn :: ExternalLayer -> ExternalLayer
addNeuronConn m = ExLayer { response = response m
                          , neurons = fmap f (neurons m) }
  where
    f :: Neuron -> Neuron
    f n = Neuron { weights = 0:weights n
                 , age = age n }

stepLayer :: ExternalLayer -> Response -> ExternalLayer
stepLayer l y = hebbianLearnLayer l newResp y
  where
    newResp = layerResponse l y

hebbianLearnLayer  :: ExternalLayer  -> Response -> Response -> ExternalLayer
hebbianLearnLayer l r y = ExLayer
                            { response = r
                            , neurons = zipWith updateNeuron r (neurons l) }
  where
    updateNeuron :: Double -> Neuron -> Neuron
    updateNeuron z' n
      | z' > 0 = Neuron { weights = updateWeights (weights n) y (learningRate n)
                        , age = age n + 1  }
      | otherwise = n

learningRate :: Neuron -> Double
learningRate n = 1 / fromIntegral (age n + 1)

layerResponse :: ExternalLayer -> Response -> Response
layerResponse l y = topK 1 $ fmap (dot y . weights) (neurons l)

neuronWidth :: ExternalLayer -> Int
neuronWidth ExLayer { neurons = ns } = length $ weights (head ns)
