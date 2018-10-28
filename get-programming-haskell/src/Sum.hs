module Sum where

import Control.Monad  
import System.Environment

toInts :: String -> [Int]
toInts = map read . lines

square :: Int -> Int
square = (^) 2

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print ((sum . map square) numbers)