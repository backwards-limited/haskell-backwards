module Playground where

data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show

data BreakfastMeat = Sausage | Bacon | Ham deriving Show

data BreakfastMain = Egg | Pancake | Waffle deriving Show

data KidsBreakfast = Kids BreakfastMain BreakfastSide deriving Show

data BasicBreakfast = Basic BreakfastMain BreakfastMeat BreakfastSide deriving Show

data LumberjackBreakfast = Lumberjack BreakfastMain BreakfastMain BreakfastMeat BreakfastMeat BreakfastSide BreakfastSide BreakfastSide deriving Show

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (foldr (||) False) . (map f)

main :: IO ()
main = do
  print $ Kids Egg Toast
  print $ Basic Egg Ham Toast
  print $ Lumberjack Egg Pancake Sausage Bacon Toast Homefries Fruit
  print (myAny (> 30) [5, 10, 15, 20])