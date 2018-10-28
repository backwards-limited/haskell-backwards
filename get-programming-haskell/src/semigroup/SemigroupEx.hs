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

module SemigroupEx where

import Data.Semigroup

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown
  deriving (Show, Eq)

instance Semigroup Color where
  a <> b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

main = do
  print(Blue <> Red)
  print("Following 2 examples show that current Semigroup implementation for Color is no associative")
  print("Green from: (Green <> Blue) <> Yellow ===> " ++ show((Green <> Blue) <> Yellow))
  print("Green from: Green <> (Blue <> Yellow) ===> " ++ show(Green <> (Blue <> Yellow)))
