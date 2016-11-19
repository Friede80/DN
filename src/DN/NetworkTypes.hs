module DN.NetworkTypes where

type Response = [Double]

data Neuron = Neuron { weights :: [Double]
                     , age     :: Int
                     }

data YNeuron = YNeuron { topDownWeights  :: [Double]
                       , bottomUpWeights :: [Double]
                       , yAge            :: Int
                       }

newtype SensorLayer = SensorLayer [Double] deriving Show

data HiddenLayer = HiddenLayer { hResponse    :: Response
                               , hOldResponse :: Response
                               , hNeurons     :: [YNeuron]
                               } deriving Show

data MotorLayer = MotorLayer { mResponse :: Response
                             , mNeurons  :: [Neuron]
                             } deriving Show

data Network = Network HiddenLayer MotorLayer deriving Show

instance Show YNeuron where
  show YNeuron { topDownWeights=t, bottomUpWeights=b, yAge=a } =
         "topDownWeights: "++(show t)++"\nbottomUpWeights: "++(show b)++"\nage: "++(show a)++"\n"

instance Show Neuron where
  show Neuron { weights=w,age=a } =
         "weights: "++(show w)++"\nage: "++(show a)++"\n"
