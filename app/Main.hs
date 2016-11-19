module Main where

import           DN.Core
import           DN.NetworkTypes

main :: IO ()
main = do
  let net = runNewNetwork zs xs
      (Network hidden motor) = runNetwork net testZs xs
  print $ "Done " ++ (show (length (hResponse hidden)))

bkgZ = [1,0,0,0]
z1 = [0,1,0,0]
z2 = [0,0,1,0]
z3 = [0,0,0,1]
zs = replicate 3 bkgZ ++ concatMap fz [z1,z2,z3] ++ replicate 1 bkgZ
fz z = replicate 2 z ++ replicate 1 bkgZ

testZs = bkgZ:bkgZ:(replicate 20 [0,0,0,0])

bkgX = [0,1,0]
x1 = [0,0,1]
x2 = [1,0,1]
x3 = [1,1,0]
xs = replicate 1 bkgX ++ concatMap fx [x1,x2,x3] ++ replicate 3 bkgX
fx x = replicate 2 x ++ replicate 1 bkgX

testXs = concatMap fx [x1,x2] ++ replicate 2 bkgX
