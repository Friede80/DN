module DN.NetworkTypes where

type Response = [Double]

data Neuron = Neuron { weights :: [Double]
                     , age :: Int
                     } deriving Show

data YNeuron = YNeuron { topDownWeights :: [Double]
                       , bottomUpWeights :: [Double]
                       , yAge :: Int
                       } deriving Show

newtype SensorLayer = SensorLayer [Double] deriving Show

data HiddenLayer = HiddenLayer { hResponse :: Response
                               , hNeurons :: [YNeuron]
                               } deriving Show

data MotorLayer = MotorLayer { mResponse :: Response
                             , mNeurons :: [Neuron]
                             } deriving Show

data Network = Network HiddenLayer MotorLayer deriving Show
