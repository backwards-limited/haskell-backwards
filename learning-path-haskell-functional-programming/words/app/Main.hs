module Main where

import System.IO
import System.Random
import Data
import Lib

main :: IO ()
main = do
  gen <- newStdGen
  let
    filledInGrid = fillInBlanks gen grid
    game = makeGame filledInGrid languages
  hSetBuffering stdout NoBuffering -- Buffering for ghc and ghci behave differently
  play game

play game = do
  putStrLn . formatGame $ game
  putStr "Enter a word > "
  word <- getLine
  let newGame = playGame game word
  if completed newGame then
    putStrLn "Congratulations!"
  else
    play newGame
  