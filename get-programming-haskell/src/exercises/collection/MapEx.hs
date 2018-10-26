module MapEx (
    organCatalog,
    availableOrgans,
    brainReport,
    spleenReport,
    processRequest
  ) where

import qualified Data.Map.Strict as Map

{-
Maps and hash tables
Maps (or dictionaries) are similar to another data structure called a hash table.
Both allow you to look up values with keys.
The big difference between these two structures is the way the values are looked up.
In a hash table, a function transforms your key into the index of an array where the value is stored.
This allows for a fast lookup of items, but requires a large amount of memory to store in order to prevent collisions.
A map looks up values by using a binary search tree.
This is slower than a hash table but still fast.
The map looks up values by searching the keys needed to have the property of being of class Ord, so you can compare two keys and efficiently find them in the tree.
-}

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog =
  map getContents ids
  where getContents = \ id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans =
  getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available =
  length (filter (\ x -> x == Just organ) available)

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation c @ (Vat a) = (Lab, c)
placeInLocation c @ (Cooler a) = (Lab, c)
placeInLocation c @ (Bag a) = (Kitchen, c)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) =
  show container ++ " in the " ++ show location

brainReport = report (process Brain)

spleenReport = report (process Spleen)

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport  Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog =
  processAndReport organ
  where organ = Map.lookup id catalog

main =
  print $ Map.lookup 7 organCatalog