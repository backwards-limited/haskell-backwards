module ApplicativeEx where

{-
Calculate the distance between 2 points on the globe.
Because a globe curves, you can’t calculate the straight-line distance between two points.
Instead, you need to use the Haversine formula.

Since we are going to ask for 2 cities which may not be in our DB, then we have to deal with missing data (such as a Maybe).
This is where Functor struggles.

fmap :: Functor f => (a -> b) -> f a -> f b

The fmap function takes any function from type a to type b, and the value of type a in the context of a Functor (like Maybe),
and returns a value of type b in the same context.
If you think of the problem in terms of types, this is pretty close.
The major difference is you have one extra argument. What you want to do is this:

1 Take haversine, which is (LatLong -> LatLong -> Double).
2 Take two arguments of type Maybe: Maybe LatLong -> Maybe LatLong.
3 And finally, you want your answer in a Maybe: Maybe Double.

This leads to the following series of type transformations:

(LatLong -> LatLong -> Double) ->
                          (Maybe LatLong ->  Maybe LatLong -> Maybe Double)

If you translate this to a more generic type signature, you get the following:
Functor f => (a -> b -> c) -> f a -> f b -> f c

This is nearly identical to fmap, except you’re adding one argument.
This is one of the limitations of Functor’s fmap: it only works on single-argument functions.
Because your main problem is having an extra argument, using partial application should move you close to a solution.

So, the problem you need to solve now is generalizing Functor’s fmap to work with multiple arguments.
Because partial application using fmap gives:
maybeInc = (+) <$> Just 1
equates to
maybeInc = fmap (+) Just 1
which is
maybeInc :: Maybe (Integer -> Integer)

The (+) operator is a function that takes two values;
by using <$> on a Maybe value, you created a function waiting for a missing value, but it’s inside a Maybe.
You now have a Maybe function, but there’s no way to apply this function - it's inside our context.

This is solved by the Applicative <*> (named "app") operator.
Applicative’s <*> allows you to apply a function in a context:

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
-}

import qualified Data.Map.Strict as Map

type LatLong = (Double, Double)

locationsDB :: Map.Map String LatLong
locationsDB = Map.fromList [
    ("Arkham", (42.6054, -70.7829)),
    ("Innsmouth", (42.8250, -70.8150)),
    ("Carcosa", (29.9714, -90.7694)),
    ("New York", (40.7776, -73.9691))
  ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 3961.0

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing i2 = i2
addMaybe i1 Nothing = i1
addMaybe (Just i1) (Just i2) = Just (i1 + i2)

newYork :: LatLong
newYork = (40.7776, -73.9691)

distanceFromNY :: LatLong -> Double
distanceFromNY = haversine newYork

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationsDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationsDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance