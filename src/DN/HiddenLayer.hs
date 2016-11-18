module DN.HiddenLayer where

import DN.NetworkTypes
import DN.Utils

import Debug.Trace
import Data.List
import Data.Ord

hebbianLearnHidden :: Response -> Response -> Response -> HiddenLayer -> HiddenLayer
hebbianLearnHidden x y z (HiddenLayer hidden)
  | length hidden == length y = trace "No new neuron" $ HiddenLayer $ zipWith updateNeuron y hidden
  | otherwise = trace "Adding new neuron" $ HiddenLayer $ zipWith updateNeuron y (newNeuron2 : hidden)
  where
    newNeuron2 = Neuron2 { topDownWeights = z
                         , bottomUpWeights = x
                         , age2 = 0 }
    updateNeuron r n
      |  r > 0.99 = Neuron2 { topDownWeights =
                                updateHiddenWeights (topDownWeights n) z (amnesiacLearnRate n)
                            , bottomUpWeights =
                                updateHiddenWeights (bottomUpWeights n) x (amnesiacLearnRate n)
                            , age2 = age2 n + 1 }
      | otherwise = n

updateHiddenWeights :: [Double] -> [Double] -> Double -> [Double]
updateHiddenWeights w sig learningRate = zipWith learn w sig
  where
    learn w s = (1 - learningRate) * w + (learningRate * s)

amnesiacLearnRate :: Neuron2 -> Double
amnesiacLearnRate n = (1+mu) / a
  where
    a :: Double
    a = fromIntegral (age2 n) + 1
    mu :: Double
    mu | a < t1 = 0
       | a < t2 = c * (a - t1) / (t2-t1)
       | otherwise = c * (a - t2) / gamma;

topDownResponse :: Response -> HiddenLayer -> Response
topDownResponse z (HiddenLayer y) = fmap (dot z . topDownWeights) y

bottomUpResponse :: Response -> HiddenLayer -> Response
bottomUpResponse x (HiddenLayer y) = fmap (dot x . bottomUpWeights) y

hiddenResponse :: Response -> Response -> HiddenLayer -> Response
hiddenResponse z x y = traceShow (tdr,bur) $ hiddenTopK 1 $ zipWith (+) tdr bur
  where
    tdr = topDownResponse z y
    bur = bottomUpResponse x y

-- Pefroms regular topK competition, but adds a new neuron if there is no
-- perfect match
-- TODO Set max neurons
hiddenTopK :: Int -> [Double] -> [Double]
hiddenTopK k ys = if isPerfectMatch
                    then map (max 0 . normalize min' max') ys
                    else hiddenTopK k (2:ys)
  where
    isPerfectMatch = traceShow ("ys",ys) $ any (>1.99) ys
    sorted = sortBy (comparing Down) ys
    max' = head sorted
    min' = sorted!!k
