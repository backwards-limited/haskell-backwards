module Data (
    Game(Game, gameGrid, gameWords),
    Cell(Cell, Indent),
    Grid,
    grid,
    languages
  ) where

import qualified Data.Map as M

-- data Game = Game (Grid Cell) (M.Map String (Maybe [Cell]))
--             deriving Show

data Game = Game {
              gameGrid :: Grid Cell,
              gameWords :: M.Map String (Maybe [Cell])
            } deriving Show

type Grid a = [[a]]

data Cell =
  Cell (Integer, Integer) Char |
  Indent
  deriving (Eq, Ord, Show)

grid :: Grid Char
grid = [
    "__C________R___",
    "__SI________U__",
    "__HASKELL____B_",
    "__A__A_____S__Y",
    "__R___B___C____",
    "__PHP____H_____",
    "____S_LREP_____",
    "____I__M_Y__L__",
    "____L_E__T_O___",
    "_________HB____",
    "_________O_____",
    "________CN_____"
  ]

languages :: [String]
languages = [
    "BASIC",
    "COBOL",
    "CSHARP",
    "HASKELL",
    "LISP",
    "PERL",
    "PHP",
    "PYTHON",
    "RUBY",
    "SCHEME"
  ]