import Test.QuickCheck
import Lib

prop_topK :: Int -> [Double] -> Bool
prop_topK _ [] = True
prop_topK k xs = length (filter (>0) (topK k xs)) == k

main = quickCheck prop_topK
