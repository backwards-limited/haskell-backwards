module ApplicativeExample1 where

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree v1 v2 v3 = min v1 (min v2 v3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

minMaybes :: Maybe Int
minMaybes = minOfThree <$> Just 10 <*> Just 3 <*> Just 6

main :: IO ()
main = do
  putStrLn "Enter 3 numbers"
  minInt <- minOfInts
  putStrLn (show minInt ++ " is the smallest")
  putStrLn ("Min of maybes = " ++ show minMaybes)