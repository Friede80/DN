module DN.Homework where

import DN.Core
import Data.Tuple
import Data.List

--------------------------------------------------------------------------------
-- Homework 4 Core Function
--------------------------------------------------------------------------------
hw4 :: String -> String -> IO ()
hw4 dn dn' = do
  let (zs, xs, _) = buildInputs dn
      (zs', xs', cleanXs) = buildInputs dn'
      net = runNewNetwork zs xs
      resubResults = testNetwork net zs xs
      thinkResults = testNetwork net zs' xs'

      (resubSense,resubMotor) = unzip resubResults
      resubMotorResults = zipWith (==) resubMotor zs
      resubSenseResults = zipWith (==) resubSense xs

      (thinkSense,thinkMotor) = unzip thinkResults
      thinkMotorResults = zipWith (==) thinkMotor zs'
      thinkSenseResults = zipWith (==) thinkSense cleanXs
  putStrLn $ "Train"
  putStrLn $ "Z State Sequence: " ++ (formatState (zip zs xs))
  putStrLn $ ""
  putStrLn $ "Resubstitution"
  putStrLn $ "Emerged Z States: " ++ (formatState (zip resubMotor resubSense))
  putStrLn $ "Z Error Rate: " ++ (show (errorRate resubMotorResults))
  putStrLn $ ""
  putStrLn $ "Thinking"
  putStrLn $ "Emerged Z and X States: " ++ (formatState (zip thinkMotor thinkSense))
  putStrLn $ "Z Error Rate: " ++ (show (errorRate thinkMotorResults))
  putStrLn $ "X Error Rate: " ++ (show (errorRate thinkSenseResults))

--------------------------------------------------------------------------------
-- Homework 4 Data
--------------------------------------------------------------------------------

dn1 = "young cat looks meal time full"
dn1' = "young cat' looks' meal' time' full' young' cat' looks' meal' time' full'"

dn2 = "young young cat well kitten kitten looks meal time hungry hungry full"
dn2' = "young young cat well kitten' looks meal' time' hungry' full"

dn3 = "young cat stares young cat looks full kitten looks meal time full hungry hungry meal time hungry cat"
dn3' = "kitten looks' meal time' hungry' hungry' hungry' cat meal time' hungry' cat"

z1 = [1,0,0,0,0,0]
z2 = [0,1,0,0,0,0]
z3 = [0,0,1,0,0,0]
z4 = [0,0,0,1,0,0]
z5 = [0,0,0,0,1,0]
z6 = [0,0,0,0,0,1]

index z
  | z == z1 = 0
  | z == z2 = 1
  | z == z3 = 2
  | z == z4 = 3
  | z == z5 = 4
  | z == z6 = 5

transitionTable = [ ("cat",    [z1,z3,z1,z1,z1,z1])
                  , ("full",   [z1,z1,z1,z1,z1,z1])
                  , ("hungry", [z6,z6,z6,z6,z6,z6])
                  , ("kitten", [z3,z3,z3,z3,z3,z3])
                  , ("looks",  [z1,z1,z4,z1,z1,z1])
                  , ("meal",   [z5,z5,z5,z5,z5,z5])
                  , ("stares", [z1,z1,z4,z1,z1,z1])
                  , ("time",   [z1,z1,z1,z1,z6,z1])
                  , ("well",   [z1,z1,z1,z1,z6,z1])
                  , ("young",  [z2,z2,z2,z2,z2,z2]) ]

symbolTable = [ ("cat",[0,0,0,1])
              , ("full", [0,0,1,0])
              , ("hungry", [0,0,1,1])
              , ("kitten", [0,1,0,0])
              , ("looks", [0,1,0,1])
              , ("meal", [0,1,1,0])
              , ("stares", [0,1,1,1])
              , ("time", [1,0,0,0])
              , ("well", [1,0,0,1])
              , ("young", [1,0,1,0])]
symbolTable' = map swap symbolTable

--------------------------------------------------------------------------------
-- Homework 4 Utilities
--------------------------------------------------------------------------------
formatState state = show $ tmap (zToString,xToString) state
  where
    tmap (f,g) = map (\(a,b) -> (f a, g b))

zToString :: [Double] -> String
zToString z = 'q':(show (index z + 1))

xToString :: [Double] -> String
xToString x = case lookup x symbolTable' of
                Just a -> a
                Nothing -> "Invalid"

errorRate :: [Bool] -> Double
errorRate res = 1 - ((genericLength (filter id res)) / genericLength res)

buildInputs :: String -> ([[Double]],[[Double]],[[Double]])
buildInputs s = (concatMap double zs, concatMap double xs, concatMap double cleanXs )
  where
    cleanXs = fmap (lookup'' symbolTable) symbols
    lookup'' sym x = case lookup x sym of
                      (Just a) -> a
                      Nothing -> case lookup (init x) sym of
                                  (Just a) -> a
                                  Nothing -> error "Bad input"
    double xs = xs:[xs]
    symbols = words s -- ++ ["cat"]  --Need cat to learn final transition
    xs = fmap (lookup' symbolTable) symbols
    lookup' sym x = case lookup x sym of
                      (Just a) -> a
                      Nothing -> [0,0,0,0]
    zs = z1: zipWith f symbols zs
    f x z = case lookup x transitionTable of
              (Just trans) -> trans!!(index z)
              Nothing -> case lookup (init x) transitionTable of
                          (Just trans) -> trans!!(index z)
                          Nothing -> error "Bad input"

toSymbols :: ([Double],[Double]) -> (Int,Int)
toSymbols (z,x) = (index z + 1, x' + 1)
  where
    justPatterns = map snd symbolTable
    x' = case elemIndex x justPatterns of
           Just a -> a
           Nothing -> error "Explosion"
