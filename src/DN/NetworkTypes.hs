module DN.NetworkTypes where

type Response = [Double]
data Neuron = Neuron { weights :: [Double]
                     , age :: Int
                     } deriving Show

data Neuron2 = Neuron2 { topDownWeights :: [Double]
                       , bottomUpWeights :: [Double]
                       , age2 :: Int
                       } deriving Show

newtype SensorLayer = SensorLayer [Double] deriving Show

newtype HiddenLayer = HiddenLayer [Neuron2] deriving Show

newtype MotorLayer = MotorLayer [Neuron] deriving Show

data Network = Network HiddenLayer MotorLayer deriving Show
