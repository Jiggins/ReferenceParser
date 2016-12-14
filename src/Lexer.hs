module Lexer
  ( identifier,   reserved,        operator,    reservedOp
  , charLiteral,  stringLiteral,   natural,     integer
  , float,        naturalOrFloat,  decimal,     hexadecimal
  , octal,        symbol,          whitespace,  parens
  , braces ,      brackets,        squares,     semi
  , comma,        colon,           dot,         semiSep
  , semiSep1,     commaSep,        commaSep1,   typeIdentifier
  ) where

import Text.Parsec
import Text.Parsec.Token (LanguageDef, TokenParser)
import qualified Text.Parsec.Token as Token

langDef :: LanguageDef st
langDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = lower <|> char '_'
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = Token.opLetter langDef
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.caseSensitive   = True
  , Token.reservedOpNames = []
  , Token.reservedNames   = []
}

-- Using defaults from Text.Parsec.Token
-- https://hackage.haskell.org/package/parsec-3.1.9/docs/src/Text-Parsec-Token.html#makeTokenParser

lexer :: TokenParser st
lexer = Token.makeTokenParser langDef

identifier, operator :: Parsec String u String
identifier = Token.identifier lexer
operator   = Token.operator lexer

reserved, reservedOp :: String -> Parsec String u ()
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer

charLiteral ::  Parsec String u Char
charLiteral = Token.charLiteral lexer

stringLiteral :: Parsec String u String
stringLiteral = Token.stringLiteral lexer

natural, integer ::  Parsec String u Integer
natural = Token.natural lexer
integer = lexeme int

float ::  Parsec String u Double
float = Token.float lexer

naturalOrFloat :: Parsec String u (Either Integer Double)
naturalOrFloat = Token.naturalOrFloat lexer

decimal, hexadecimal, octal ::  Parsec String u Integer
decimal     = Token.decimal lexer
hexadecimal = Token.hexadecimal lexer
octal       = Token.octal lexer

symbol :: String -> Parsec String u String
symbol = Token.symbol lexer

lexeme :: Parsec String u a -> Parsec String u a
lexeme = Token.lexeme lexer

whitespace ::  Parsec String u ()
whitespace = Token.whiteSpace lexer

parens, braces, brackets, squares :: Parsec String u a -> Parsec String u a
parens   = Token.parens lexer
braces   = Token.braces lexer
brackets = Token.brackets lexer
squares  = Token.brackets lexer

semi, comma, colon, dot ::  Parsec String u String
semi  = Token.semi lexer
comma = Token.comma lexer
colon = Token.colon lexer
dot   = Token.dot lexer

semiSep, semiSep1, commaSep, commaSep1 :: Parsec String u a -> Parsec String u [a]
semiSep   = Token.semiSep lexer
semiSep1  = Token.semiSep1 lexer
commaSep  = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer

-- | Type identifiers differ from identifiers in that they start with
-- an uppercase letter.
typeIdentifier :: Parsec String u String
typeIdentifier = lexeme (
  do { c  <- upper
     ; cs <- many (Token.identLetter langDef)
     ; return (c:cs)
     })
  <?> "type"

-- | I didn't like the default integer token parsing `+1` as (1 :: Int) as it
-- makes it difficult to parse arithmetic.
int :: Parsec String u Integer
int = natural
   <|> fmap negate (char '-' *> natural)
   <?> "expecting integer"


