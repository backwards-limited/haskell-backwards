{-# LANGUAGE OverloadedStrings #-}

module Data (
  MarcRecordRaw, MarcLeaderRaw,
  Html, Author, Title,
  Book(Book, title, author),
  leaderLength,
  book1, book2, book3,
  books
) where

import qualified Data.ByteString as B
import qualified Data.Text as T  

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

type Html = T.Text

type Author = T.Text
type Title = T.Text

data Book = Book {
  author :: Author,
  title :: Title
} deriving Show

leaderLength :: Int
leaderLength = 24

book1 :: Book
book1 = Book {
  title = "The Conspiracy Against the Human Race",
  author = "Ligotti, Thomas"
}

book2 :: Book
book2 = Book {
  title = "A Short History of Decay",
  author = "Cioran, Emil"
}

book3 :: Book
book3 = Book {
  title = "The Tears of Eros",
  author = "Bataille, Georges"
} 

books :: [Book]
books = [book1, book2, book3]