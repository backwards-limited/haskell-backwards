# Words

## Building

The build file (much like a Scala build.sbt) is a .cabal file.
So any 3rd party dependencies are added to this file.

words.cabal has been updated where:

- words.exe changed to words
- Added -dynamic to ghc-options to keep generated executable to its smallest size
- Added dependency on hspec

```haskell
stack build

stack exec words

stack test
```

## Hoogle

Let's say we want to display our words grid in a nice format.
The grid is a [String] and we want a nicely formatted String.
What we want is a function [String] -> String.

Well, we can simply put this type signature into [https://www.haskell.org/hoogle](Hoogle) to see what is available.