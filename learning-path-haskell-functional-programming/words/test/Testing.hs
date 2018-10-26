import Control.Monad (guard)

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)

-- Sample coordinate grid - a goal is to generate programmatically
coords = [
    [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)],
    [(1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7)],
    [(2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7)],
    [(3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7)],
    [(4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7)],
    [(5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (5, 7)],
    [(6, 0), (6, 1), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7)],
    [(7, 0), (7, 1), (7, 2), (7, 3), (7, 4), (7, 5), (7, 6), (7, 7)]
  ]

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

-- Variant of "outputGrid" for arbitary Show-able structures
og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

-- Check if divisible by 2
div2 x = x `mod` 2 == 0

-- List monad notation
mapped = do
  i <- [0 ..]
  return (i * 2)

filtered = do
  i <- [0 ..]
  guard (div2 i)
  return i

mappedAndFiltered = do
  i <- [0 .. 9]
  guard (div2 i)
  return (i * 2)

-- Above has the equivalent "list comprehension"
listComprehensionMappedAndFiltered = [ i * 2 | i <- [0 .. 9], div2 i ]

coords1 = do
  row <- [0 .. 7]
  return $ do
    col <- [0 .. 7]
    return (row, col)

coords2 = [ [(row, col) | col <- [0 .. 7]] | row <- [0 .. 7] ]

repeat8 = take 8 . repeat

cols8 = repeat8 [0 .. 7]

rows8 = map repeat8 [0 .. 7]

coords3 = zipWith zip rows8 cols8

zipOverGrid = zipWith zip
{-
E.g.
zipOverGrid coords grid
gives
[[((0,0),'_'),((0,1),'_'),((0,2),'C'), .....
-}

cellOverGrid = zipWith (zipWith Cell)
{-
E.g.
cellOverGrid coords grid
gives
[[Cell (0,0) '_',Cell (0,1) '_',Cell (0,2) 'C', ......
-}

-- Generalise the above
{-
Iterations to simplifying the function:

zipOverGridWith f a b = (zipWith (zipWith f)) a b

zipOverGridWith f = zipWith (zipWith f)

zipOverGridWith f = zipWith . zipWith $ f

..... to end up with
-}
zipOverGridWith = zipWith . zipWith

example = og $ zipOverGridWith Cell coords grid