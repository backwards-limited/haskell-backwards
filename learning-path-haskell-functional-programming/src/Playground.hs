module Playground where

data Compass = North | East | South | West

instance Show Compass where
  show North = "North"
  show East = "East"
  show South = "South"
  show West = "West"

instance Eq Compass where
  North == North = True
  _ == _ = False 

main = do
  print North
  print (North == North)
  print (South == North)