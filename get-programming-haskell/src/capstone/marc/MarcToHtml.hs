{-# LANGUAGE OverloadedStrings #-}

module MarcToHtml where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe
import Data

-- Try: GHCi> bookToHtml book1
bookToHtml :: Book -> Html
bookToHtml book =
  mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
        authorInTags = mconcat ["<em>", (author book), "</em>\n"]

-- Try: GHCi> booksToHtml books      
booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat [
    "<html>\n",
      "<head><title>books</title>" ,"<meta charset='utf-8'/>" ,"</head>\n",
      "<body>\n", booksHtml, "\n</body>\n",
    "</html>"
  ]
  where booksHtml = (mconcat . (map bookToHtml)) books

main :: IO ()
main = TIO.writeFile "books.html" (booksToHtml books)