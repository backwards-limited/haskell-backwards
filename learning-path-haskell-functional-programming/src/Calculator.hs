module Calculator where

data Expression =
  Number Int |
  Add Expression Expression |
  Subtract Expression Expression
  deriving (Eq, Ord, Show)

calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add e1 e2) = (calculate e1) + (calculate e2)
calculate (Subtract e1 e2) = (calculate e1) - (calculate e2)

main = do
  print (Number 1)
  print (calculate(Number 1))
  print (Number 1 == Number 1)
  print (Number 2 > Number 1)
  print (Add (Number 1) (Number 10))
  print (calculate(Add (Number 1) (Number 10)))
  print (Add (Number 1) (Subtract (Number 10) (Number 2)))
  print (calculate(Add (Number 1) (Subtract (Number 10) (Number 2))))