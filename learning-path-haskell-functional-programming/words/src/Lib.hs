module Lib (
    makeGame,
    totalWords,
    score,
    playGame,
    formatGame,
    completed,
    makeRandomGrid,
    fillInBlanks,
    zipOverGrid,
    zipOverGridWith,
    gridWithCoords,
    outputGrid,
    formatGrid,
    cell2char,
    findWords,
    findWord,
    findWordInCellLinePrefix,
    skew
  ) where

import Data.Char (toLower)
import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M
import System.Random
import Data

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let
    gwc = gridWithCoords grid
    tuplify word = (word, Nothing)
    list = map tuplify words
    dict = M.fromList list
  in
    Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) =
  game
playGame game word =
  let
    grid = gameGrid game
    foundWord = findWord grid word
  in case foundWord of
    Nothing ->
      game
    Just cs ->
      let
        dict = gameWords game
        newDict = M.insert word foundWord dict
      in
        game { gameWords = newDict }

formatGame :: Game -> String
formatGame game =
  formatGameGrid game ++
  "\n" ++
  (show $ score game) ++
  " / " ++
  (show $ totalWords game) ++
  "\n"

completed :: Game -> Bool
completed game =
  score game == totalWords game

makeRandomGrid gen =
  let
    (gen1, gen2) = split gen
    row = randomRs ('A', 'Z') gen1
  in
    row : makeRandomGrid gen2

fillInBlanks gen grid =
  let
    r = makeRandomGrid gen
    fill '_' r = r
    fill c _ = c
  in
    zipOverGridWith fill grid r

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let
    rows = map repeat [0 .. ]
    cols = repeat [0 .. ]
  in
    zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO ()
outputGrid = putStrLn . formatGrid 

formatGameGrid :: Game -> String
formatGameGrid game =
  let
    grid = gameGrid game
    dict = gameWords game :: M.Map String (Maybe [Cell])
    cellSet = concat . catMaybes . M.elems $ dict
    formatCell cell =
      let
        char = cell2char cell
      in
        if cell `elem` cellSet then char else toLower char
    charGrid = mapOverGrid formatCell grid
  in
    unlines charGrid

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words = catMaybes $ map (findWord grid) words

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let
    horizontal = grid
    vertical = transpose horizontal
    diagonalUp = diagonalise horizontal
    diagonalDown = diagonalise $ map reverse horizontal
    lines = horizontal ++ vertical ++ diagonalUp ++ diagonalDown
    foundWords = map (findWordInLine word) (lines ++ (map reverse lines))
  in listToMaybe (catMaybes foundWords)

  --   found = or $ map (isInfixOf word) (lines ++ (map reverse lines))
  -- in
  --   if found then Just word else Nothing  

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    cs @ (Just _) -> cs
    Nothing -> findWordInLine word (tail line)

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x : xs) (c : cs) | x == cell2char c =
  findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l : ls) = l : skew (map indent ls)
  where indent line = Indent : line

diagonalise :: Grid Cell -> Grid Cell
diagonalise = transpose . skew