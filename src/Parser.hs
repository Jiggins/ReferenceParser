module Parser where

import Text.Parsec
import Data.Char
import Data.List (intercalate)
import Data.Functor.Identity

import Lexer
import Reference

testReference = "Christie, F. (1985). Language and schooling. In S. Tchudi (Ed.), Language, Schooling and Society (pp 21–40). Upper Montclaire, NJ, Boynton/Cook."

test p = parse p "" testReference

reference = Reference <$> lookAhead (manyTill anyChar eof)
                      <*> fmap (intercalate ", ") author
                      <*> year
                      <*> title
                      <*> many feature
                      <*> many anyChar

author :: Parsec String u [String]
author = manyTill punctuatedSentence ((lookAhead year *> pure ()) <|> eof)

punctuatedSentence :: Parsec String u String
punctuatedSentence = many punctuated

punctuated :: Parsec String u Char
punctuated =  letter
          <|> fmap head comma
          <|> fmap head dot
          <|> char ' '
          <|> fmap head (symbol "&")
          <|> unicode

unicode :: Parsec String u Char
unicode = oneOf "\8217"

year :: Parsec String u Integer
year = read <$> parens (count 4 digit <* optional letter) <* dot

title :: Parsec String u String
title = manyTill titleChar (dot *> return () <|> eof)
  where titleChar = letter <|> digit <|> fmap head comma <|> char ' ' <|> unicode

-- * Features

-- features = option [] (do {f <- feature; return f : features})

feature :: Parsec String u Feature
feature =  editor
       <|> volume
       <|> pages
       <|> bookTitle

bookTitle = BookTitle . unwords . head  <$> endBy1 (many1 word) (lookAhead otherFeature)
  where otherFeature = editor <|> volume <|> pages

editor :: Parsec String u Feature
editor = Editor <$> between (string "In ") (parens (many eds)) punctuatedSentence
  where eds =  char 'E' <|> char 'e'
            *> char 'D' <|> char 'd'
            <* optional (char 'S' <|> char 's')
            <* optional dot

volume :: Parsec String u Feature
volume = Volume <$> (volume' *> integer)
  where volume' =  string "Volume " <|> string "volume "
               <|> string "Vol. "   <|> string "Vol. "

pages :: Parsec String u Feature
pages =  optional (char '(') *> (try singlePage <|> multiPage) <* optional (char '(')
  where singlePage = Page <$> (string "p." *> whitespace *> integer)
        multiPage  = PageRange <$> ((optional (string "pp") *> optional dot *> whitespace)
                                *> (integer <* whitespace <* dash <* whitespace))
                               <*> integer

-- * Util

word :: Parsec String u String
word = whitespace *> many1 (letter <|> fmap head comma <|> fmap head dot) <* whitespace

dash :: Parsec String u Char
dash = oneOf "-\8211"

