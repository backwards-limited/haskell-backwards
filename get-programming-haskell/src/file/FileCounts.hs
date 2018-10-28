module FileCounts where

import System.Environment
import System.IO

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
 where
  charCount = length input
  wordCount = (length . words) input
  lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
  unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

{-
Try out the above on the REPL:
GHCi>  (countsText . getCounts) "this is\n some text"
 "chars: 18 words: 4 lines: 2"
-}

-- WATCH OUT FOR BUGS DUE TO LAZY EVALUATION - so the order of statements can become very important
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file                                      -- "input" still hasn't been evaluated yet
  let summary = (countsText . getCounts) input                    -- Even though ""summary is defined, it hasn't been used - neither "summary" nor "input" have been evaluated
  putStrLn summary                                                -- "putStrLn" needs to print "summary"; this forces "summary" to be evaluated and thus the "input" to be read in so it can be used by "summary"
  hClose file                                                     -- Now closing the file causes no problem because the value inside "summary" has been evaluated
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"]) -- Appending the file works as expected; your file will be updated correctly