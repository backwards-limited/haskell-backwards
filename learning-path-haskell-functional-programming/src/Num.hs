module Num where

import Data.Complex
import Data.Ratio

n0 :: Int
n0 = 5

n1 :: Double
n1 = 5.0

n2 :: Complex Double
n2 = 2 :+ 3

n3 :: Ratio Int
n3 = 2 % 3

main :: IO ()
main = do

  print n0
  print n1
  print n2
  print n3