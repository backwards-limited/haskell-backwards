module Lib where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

outputGrid :: Grid -> IO ()
outputGrid = putStrLn . formatGrid

formatGrid :: Grid -> String
formatGrid = unlines

-- findWords :: Grid -> [String] -> [Bool]
findWords grid words = catMaybes $ map (findWord grid) words

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let
    horizontal = grid
    vertical = transpose horizontal
    diagonalUp = diagonalise horizontal
    diagonalDown = diagonalise $ map reverse horizontal
    lines = horizontal ++ vertical ++ diagonalUp ++ diagonalDown
    found = or $ map (isInfixOf word) (lines ++ (map reverse lines))
  in
    if found then Just word else Nothing  

skew :: Grid -> Grid
skew [] = []
skew (l : ls) = l : skew (map indent ls)
  where indent line = '_' : line

diagonalise :: Grid -> Grid
diagonalise = transpose . skew

type Grid = [String]

grid :: Grid
grid =
  [ "__C________R___"
  , "__SI________U__"
  , "__HASKELL____B_"
  , "__A__A_____S__Y"
  , "__R___B___C____"
  , "__PHP____H_____"
  , "____S_LREP_____"
  , "____I__M_Y__L__"
  , "____L_E__T_O___"
  , "_________HB____"
  , "_________O_____"
  , "________CN_____"
  ]

languages :: [String]
languages =
  [ "BASIC"
  , "COBOL"
  , "CSHARP"
  , "HASKELL"
  , "LISP"
  , "PERL"
  , "PHP"
  , "PYTHON"
  , "RUBY"
  , "SCHEME"
  ]

zipOverGrid = zipWith zip


repeat8 = take 8 . repeat

cols8 = repeat8 [0 .. 7]

rows8 = map repeat8 [0 .. 7]

grid8 = zipOverGrid rows8 cols8


cols = repeat [0 ..]

rows = map repeat [0 ..]

coords = zipOverGrid rows cols
