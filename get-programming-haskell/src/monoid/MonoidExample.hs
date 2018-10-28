-- Create probability tables for events and have an easy way to combine them.

module MonoidExampe where

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

-- Function will be a basic constructor, but it’ll also ensure that your probabilities sum to 1.
createPTable :: Events -> Probs -> PTable
createPTable events probs =
  PTable events normalizedProbs where
    totalProbs = sum probs
    normalizedProbs = map (\ x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob =
  mconcat [event, "|", show prob, "\n"]

{- Remember that
GHCi> zipWith (+) [1, 2, 3] [4, 5, 6]
 [5,7,9]
-}

instance Show PTable where
  show (PTable events probs) =
    mconcat pairs where
      pairs = zipWith showPair events probs

{-
What you want to be able to model using the Monoid type class is the combination of two (or more) PTables.
For example, if you have two coins, you want an outcome like this:
 heads-heads|0.25
 heads-tails|0.25
 tails-heads|0.25
 tails-tails|0.25
-}

{-
This requires generating a combination of all events and all probabilities. This is called the Cartesian product.
-}

-- You’ll start with a generic way to combine the Cartesian product of 2 lists with a function
cartesianCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianCombine func l1 l2 =
  zipWith func newL1 cycledL2 where
    nToAdd = length l2 -- You need to repeat each element in the first list once for each element in the second
    repeatedL1 = map (take nToAdd . repeat) l1 -- Maps l1 and makes nToAdd copies of the element
    newL1 = mconcat repeatedL1 -- The preceding line leaves you with a lists of lists, and you need to join them
    cycledL2 = cycle l2 -- By cycling the second list, you can use zipWith to combine these two lists

-- Then your functions for combining events and combining probabilities are specific cases of cartCombine
combineEvents :: Events -> Events -> Events
combineEvents e1 e2 =
  cartesianCombine combiner e1 e2 where
    combiner = (\ x y -> mconcat [x, "-", y]) -- When combining events, you hyphenate the event names

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 =
  cartesianCombine (*) p1 p2 -- To combine probabilities, you multiply them

instance Semigroup PTable where
  ptable1 <> (PTable [] []) = ptable1 -- Handle the special case of having an empty PTable.
  (PTable [] []) <> ptable2 = ptable2 -- Handle the special case of having an empty PTable.
  (PTable e1 p1) <> (PTable e2 p2) = createPTable newEvents newProbs
      where newEvents = combineEvents e1 e2
            newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

{-
Let’s see how to create two PTables.
The first is a fair coin, and the other is a color spinner with different probabilities for each spinner.
-}
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

main = do
  print (createPTable ["heads", "tails"] [0.5, 0.5])
  {- Will give us:
  heads|0.5
  tails|0.5
  -}

  -- If you want to know the probability of getting tails on the coin and blue on the spinner, you can use your <> operator
  print (coin <> spinner)
  {- Will give us:
  heads-red|5.0e-2
  heads-blue|0.1
  heads-green|0.35
  tails-red|5.0e-2
  tails-blue|0.1
  tails-green|0.35
  -}

  -- Probability of flipping heads three times in a row?
  print (mconcat [coin,coin,coin])
  {-
  heads-heads-heads|0.125
  heads-heads-tails|0.125
  heads-tails-heads|0.125
  heads-tails-tails|0.125
  tails-heads-heads|0.125
  tails-heads-tails|0.125
  tails-tails-heads|0.125
  tails-tails-tails|0.125
  -}