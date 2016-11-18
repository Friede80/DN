module Main where

import DN.Core
import DN.NetworkTypes

main :: IO ()
main = let (Network hidden motor) = runNetwork zs xs
       in print hidden
  where
    bkgZ = [1,0,0,0]
    z1 = [0,1,0,0]
    z2 = [0,0,1,0]
    z3 = [0,0,0,1]
    zs = replicate 4 bkgZ ++ concatMap fz [z1,z2,z3]
    fz z = replicate 4 z ++ replicate 2 bkgZ

    bkgX = [0,1,0]
    x1 = [0,0,1]
    x2 = [1,0,1]
    x3 = [1,1,0]
    xs = replicate 2 bkgX ++ concatMap fx [x1,x2,x3]
    fx x = replicate 4 x ++ replicate 2 bkgX
