module Glitcher where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- return imageFile
  let glitchedFileName = mconcat ["glitched-", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"