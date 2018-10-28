{-# LANGUAGE OverloadedStrings #-}

module Text where

import qualified Data.Text as T

{-
To compile, we have two choices:

Compile with the -X flag OverloadedStrings i.e
ghc -XOverloadedStrings -main-is Text Text.hs -o text

OR as we do above use a language pragma
-}
needsExtension :: T.Text
needsExtension = "blah"

main :: IO ()
main = print (T.unpack needsExtension)