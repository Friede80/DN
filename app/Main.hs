module Main where

import           DN.Homework

main :: IO()
main = do
  putStrLn "DN-1"
  hw4 dn1 dn1'
  putStrLn "DN-2"
  hw4 dn2 dn2'
  putStrLn "DN-3"
  hw4 dn3 dn3'

{--For vision processing toy problem
main = do
  let net = runNewNetwork zs' xs'
      res = testNetwork net zs' xs'
  print res

z1 = [1,0,0,0]
z2 = [0,1,0,0]
z3 = [0,0,1,0]
z4 = [0,0,0,1]
zs = [z1,z2,z3,z4]

x1 = [0,0,1]
x2 = [1,0,1]
x3 = [1,1,0]
x4 = [0,1,0]
xs = [x1,x2,x3,x4]

bkgX = head xs
bkgZ = head zs
zs' = replicate 2 bkgZ ++ concatMap (expand bkgZ) zs ++ replicate 2 bkgZ
xs' = concatMap (expand bkgX) xs ++ replicate 4 bkgX
expand bkg x = replicate 4 x ++ replicate 2 bkg
-}
