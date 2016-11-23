module DN.HiddenLayer where

import           Debug.Trace
import           DN.NetworkTypes
import           DN.Utils

maxNeurons = 100

stepHidden :: Bool -> HiddenLayer -> Response -> Response -> HiddenLayer
stepHidden frzn h x z = hebbianLearnHidden x newResp z h
  where
    newResp = hiddenResponse frzn h x z

hebbianLearnHidden :: Response -> Response -> Response -> HiddenLayer -> HiddenLayer
hebbianLearnHidden x y z h
  | length (hNeurons h) == length y = newHiddenLayer (hNeurons h)
  | otherwise = newHiddenLayer (newNeuron2 : hNeurons h)
  where
    newHiddenLayer ns = HiddenLayer { hResponse = y
                                    , hOldResponse = hResponse h
                                    , hNeurons =  zipWith updateNeuron y ns }
    newNeuron2 = YNeuron { topDownWeights = z
                         , bottomUpWeights = x
                         , yAge = 0 }
    updateNeuron r n
      |  r > 0 = YNeuron { topDownWeights =
                             updateWeights (topDownWeights n) z (amnesiacLearnRate n)
                         , bottomUpWeights =
                             updateWeights (bottomUpWeights n) x (amnesiacLearnRate n)
                         , yAge = yAge n + 1 }
      | otherwise = n

amnesiacLearnRate :: YNeuron -> Double
amnesiacLearnRate n = (1+mu) / a
  where
    a = fromIntegral (yAge n + 1)
    mu | a < t1 = 0
       | a < t2 = c * (a - t1) / (t2-t1)
       | otherwise = c * (a - t2) / gamma;

hiddenResponse :: Bool -> HiddenLayer -> Response -> Response -> Response
hiddenResponse frzn h x z = if frzn
                              then topK 1 $ zipWith (+) tdr bur
                              else hiddenTopK 1 $ zipWith (+) tdr bur
  where
    tdr = topDownResponse z h
    bur = bottomUpResponse x h

topDownResponse :: Response -> HiddenLayer -> Response
topDownResponse = neuralResponse topDownWeights

bottomUpResponse :: Response -> HiddenLayer -> Response
bottomUpResponse = neuralResponse bottomUpWeights

neuralResponse :: (YNeuron -> [Double]) -> Response -> HiddenLayer -> Response
neuralResponse dir x h = fmap (dot x . dir) (hNeurons h)

-- Pefroms regular topK competition, but adds a new neuron if there is no
-- perfect match
-- TODO Set max neurons
hiddenTopK :: Int -> [Double] -> [Double]
hiddenTopK k ys
  | length ys >= maxNeurons = topK k ys
  | any (>1.99) ys = topK k ys
  | otherwise      = topK k (2:ys)
