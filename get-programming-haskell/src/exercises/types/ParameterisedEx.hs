module ParameterisedEx (wrap, unwrap) where

data Box a = Box a deriving Show
{-    ^  ^    ^  ^
      |  |    |  |---- type variable being used
      |  |    |
      |  |    |------- data constructor
      |  |
      |  |------------ declaration of a type variable
      |
      |--------------- type constructor

The box type is an abstract container that can hold any other type.
As soon as you put a type inside Box, the Box type takes on a concrete value.
-}

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- Build in definition of List:
-- data [] a = [] | a : [a]

-- Defining our own list
data List a = Empty | Cons a (List a) deriving Show

builtInEx :: [Int]
builtInEx = 1 : 2 : 3 : []

ourListEx :: List Int
ourListEx = Cons 1 (Cons 2 (Cons 3 Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons a rest) = Cons (f a) (ourMap f rest)

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

first :: Triple a -> a 
first (Triple x _ _) = x

-- The type of a type is called its kind
-- Kinds are abstract
-- The "kind of a type" indicates the number of parameters the type takes, which are expressed using asterisk (*)
-- Types that take zero parameters have a kind of: *
-- Types that take one parameter have the kind: * -> *
-- Types with two parameters have the kind: * -> * -> *
-- etc.

{-
In GHCi, you use the :kind command to look up the kinds of any types you’re unsure of:
GHCi> import Data.Map
GHCi> :kind Int
Int :: *
GHCi> :kind Triple
Triple :: * -> *
GHCi> :kind []
[] :: * -> *
GHCi> :kind (,)
(,) :: * -> * -> *
GHCi> :kind Map.Map
Map.Map :: * -> * -> *

It’s worth pointing out that concrete types have a different kind than their nonconcrete equivalents:
GHCi> :kind [Int]
[Int] :: *
GHCi> :kind Triple Char
Triple Char :: *
-}

main :: IO ()
main = do
  print $ wrap 6
  print $ unwrap (Box 12)
  
  putStrLn ""
  print builtInEx
  print ourListEx
  
  putStrLn ""
  print (ourMap (* 2) (Cons 1 (Cons 2 (Cons 3 Empty))))