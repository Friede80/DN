module DN.NetworkTypes where

type Response = [Double]

data Neuron = Neuron { weights :: [Double]
                     , age     :: Int
                     }

data YNeuron = YNeuron { topDownWeights  :: [Double]
                       , bottomUpWeights :: [Double]
                       , yAge            :: Int
                       }

data HiddenLayer = HiddenLayer { hResponse    :: Response
                               , hOldResponse :: Response
                               , hNeurons     :: [YNeuron]
                               } deriving Show

data ExternalLayer = ExLayer { response :: Response
                             , neurons  :: [Neuron]
                             } deriving Show

type MotorLayer = ExternalLayer
type SensorLayer = ExternalLayer

data Network = Network SensorLayer HiddenLayer MotorLayer deriving Show

instance Show YNeuron where
  show YNeuron { topDownWeights=t, bottomUpWeights=b, yAge=a } =
         "topDownWeights: "++(show t)++"\nbottomUpWeights: "++(show b)++"\nage: "++(show a)++"\n"

instance Show Neuron where
  show Neuron { weights=w,age=a } =
         "weights: "++(show w)++"\nage: "++(show a)++"\n"
