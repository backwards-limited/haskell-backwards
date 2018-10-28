{-# LANGUAGE OverloadedStrings #-}

module FileCountsUsingStringText where

import           System.IO
import           System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI
getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
 where
  charCount = T.length input
  wordCount = (length . T.words) input
  lineCount = (length . T.lines) input
