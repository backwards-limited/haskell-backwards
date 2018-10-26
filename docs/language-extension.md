# Language Extension

## Using -X flag

The following example will not compile - we can fix this with a language extension -X flag:

```haskell
module Text where

import qualified Data.Text as T

needsExtension :: T.Text
needsExtension = "blah"

main :: IO ()
main = print (T.unpack needsExtension)
```

To compile the above do:

```bash
ghc -XOverloadedStrings -main-is Text Text.hs -o text
```

## Language Pragma

A better solution to the above is to use a language pragma of the format:

```haskell
{-# LANGUAGE <Extension Name> #-}
```

Let's rewrite the above code:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Text where

import qualified Data.Text as T

needsExtension :: T.Text
needsExtension = "blah"

main :: IO ()
main = print (T.unpack needsExtension)
```

Examples of other pragmas:

- ViewPatterns
  Allows for more-sophisticated pattern matching.

- TemplateHaskell
  Provides tools for Haskell metaprogramming.

- DuplicateRecordFields
  Solves the annoying problem where using the same field name for different types using record syntax causes a conflict.
- 
- NoImplicitPrelude
  Some Haskell programmers prefer to use a custom Prelude.
  This language extension allows you to not use the default Prelude.