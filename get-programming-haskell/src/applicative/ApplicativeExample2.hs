module ApplicativeExample2 where

data User = User {
  name :: String,
  gamerId :: Int,
  score :: Int
} deriving Show

user1 = User { name = "Sue", gamerId = 1337, score = 9001 }
user2 = User "Bob" 2336 875

-- Let's imagine we get data from some datastore:
serverUsername :: Maybe String
serverUsername = Just "Sue"
 
serverGamerId :: Maybe Int
serverGamerId =  Just 1337
 
serverScore :: Maybe Int
serverScore = Just 9001

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  print $ show user1
  print $ show user2
  print $ show (User <$> serverUsername <*> serverGamerId <*> serverScore)
  putStrLn "Enter a username, gamerId and score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user