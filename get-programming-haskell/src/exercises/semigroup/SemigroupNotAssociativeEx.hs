{-
Add more than two colors.
i.e. instead of the simplest semigroup such as:
Blue <> Red
we have:
(Blue <> Red) <> Yellow

You want your color mixing to be associative.
Associative means that the order in which you apply your <> operator doesn’t matter.
For numbers, this means that
1 + (2 + 3) = (1 + 2) + 3
-}

module SemigroupNotAssociativeEx where

import Data.Semigroup

instance Semigroup Integer where
  x <> y = x + y

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown
  deriving (Show, Eq)

instance Semigroup Color where
  Red <> Blue = Purple
  Blue <> Red = Purple
  Yellow <> Blue = Green
  Blue <> Yellow = Green
  Yellow <> Red = Orange
  Red <> Yellow = Orange
  a <> b =
    if a == b then a else Brown

main = do
  print("Combine (via Semigroup Integer instance) 100 and 10 ===> " ++ show (100 <> 10))
  print(Blue <> Red)
  print("Following 2 examples show that current Semigroup implementation for Color is no associative")
  print("Brown from: (Green <> Blue) <> Yellow ===> " ++ show((Green <> Blue) <> Yellow))
  print("Green from: Green <> (Blue <> Yellow) ===> " ++ show(Green <> (Blue <> Yellow)))
