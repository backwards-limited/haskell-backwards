module PatternMatching1 where

data Colour = RGB Int Int Int | CMYK Float Float Float Float deriving Show

colourModel :: Colour -> String
colourModel RGB {} = "RGB"
colourModel CMYK {} = "CMYK"

colourModel2 :: Colour -> String
colourModel2 c =
  case c of RGB {} -> "RGB" 
            CMYK {} -> "CMYK"

main :: IO ()
main = do
  let c = CMYK 1.0 2.0 3.0 4.0
  putStrLn $ colourModel c
  putStrLn $ colourModel2 c