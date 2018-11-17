module FunctorEx where

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just x) = Just (reverse x)
reverseMaybe _ = Nothing
  
betterReverseMaybe :: Maybe String -> Maybe String
betterReverseMaybe = fmap reverse

successfulRequest :: Maybe Int
successfulRequest = Just 6
 
failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

{-
instance Functor Maybe where
  fmap func (Just n) = Just (func n)
  fmap func Nothing = Nothing
-}

main :: IO ()
main = do
  print (incMaybe successfulRequest) -- which is equivalent to (avoid repetition of bespoke functions such as "incMaybe"):
  print (fmap (+ 1) successfulRequest) -- this is because of the above instance which is automatically in scope
  print ((+ 1) <$> successfulRequest) -- Though fmap is the official function name, in practice the binary operator <$> is used much more frequently
  print "-----------------------------------------------------"
  print (incMaybe failedRequest) -- which is equivalent to
  print (fmap (+ 1) failedRequest)
  print ((+ 1) <$> failedRequest)