{-# LANGUAGE OverloadedStrings #-}

module Data (
  MarcRecordRaw, MarcLeaderRaw, MarcDirectoryRaw, MarcDirectoryEntryRaw,
  FieldText,
  Html, Author, Title,
  FieldMetadata(FieldMetadata, tag, fieldLength, fieldStart),
  Book(Book, title, author),
  leaderLength, dirEntryLength,
  fieldDelimiter, titleTag, titleSubfield, authorTag, authorSubfield,
  book1, book2, book3,
  books
) where

import qualified Data.ByteString as B
import qualified Data.Text as T  

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString

type FieldText = T.Text

type Html = T.Text

type Author = T.Text
type Title = T.Text

data FieldMetadata = FieldMetadata {
  tag :: T.Text,
  fieldLength :: Int,
  fieldStart  :: Int
} deriving Show

data Book = Book {
  author :: Author,
  title :: Title
} deriving Show

leaderLength :: Int
leaderLength = 24

dirEntryLength :: Int
dirEntryLength = 12

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

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