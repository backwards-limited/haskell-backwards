{-
Basically a Semigroup that includes "identity".

The Monoid type class is also interesting because it demonstrates an annoying problem in the evolution of Haskell type classes.
Logically, you’d assume that the definition of Monoid would look like the following:

class Semigroup a => Monoid a where
  identity :: a

After all, Monoid should be a subclass of Semigroup because it’s just Semigroup with identity.
But Monoid predates Semigroup and isn’t officially a subclass of Semigroup.
Instead, the definition of Monoid is perplexing:

class Monoid a where
  mempty :: a                       -- instead of "identity"
  mappend :: a -> a -> a            -- instead of "<>"
  mconcat :: Monoid a => [a] -> a   -- this is defined for us as: mconcat = foldr mappend mempty
-}

module MonoidEx where

instance Semigroup Integer where
  x <> y = x * y

instance Monoid Integer where
  mempty = 1
  -- mappend x y = x * y

main = do
  -- Each method (++, <>, mappend) is called according to its type e.g. regarding a list, use mappend defined by "instance Monoid []"
  print ([1, 2, 3] ++ [])
  print ([1, 2, 3] <> [])
  print ([1, 2, 3] `mappend` mempty)

  print (10 <> 10)
  print (10 <> mempty)

  -- The mconcat method takes a list of Monoids and combines them, returning a single Monoid.
  -- The best way to understand mconcat is by taking a list of lists and seeing what happens when you apply mconcat.
  -- To make things easier, you’ll use strings because those are just lists of Chars:
  print (mconcat ["does", " this", " make", " sense?"])
