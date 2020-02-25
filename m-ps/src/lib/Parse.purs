module Parse where

import Prelude
import Tree
import Control.Monad
import Data.Functor
import Control.Apply
import Text.Parsing.Parser (ParseError, runParser, fail)
import Text.Parsing.Parser (Parser) as P
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token
import Text.Parsing.Parser.Combinators
import Data.Either
import Data.List hiding (many, some)
import Data.Array (many, some)
import Data.Char
import Data.Maybe
import Control.Alternative
import Data.Int (fromString)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)

type Parser a
  = P.Parser String a

lineCommentTail :: Parser Unit
lineCommentTail = void $ many $ noneOf [ '\n' ]

blockCommentTail :: Parser Unit
blockCommentTail = do
  void $ char '('
  void $ many (blockCommentTail <|> void (noneOf [ '(', ')' ]))
  void $ char ')'

comment :: Parser Unit
comment = char '#' *> (blockCommentTail <|> lineCommentTail)

ignored :: Parser Unit
ignored = skipMany (comment <|> void space)

opChar :: Parser Char
opChar = oneOf (toCharArray "~`!@$%^&*-=+\\|;:<>,.?/")

symbolChar :: Parser Char
symbolChar = letter

symbolCdr :: Parser String
symbolCdr = many (symbolChar <|> digit <|> opChar) <#> fromCharArray

escapedQuote :: Parser Char
escapedQuote = do
  void $ string "\"\""
  pure '\"'

parseSymbol :: Parser Tree
parseSymbol = do
  car <- symbolChar
  cdr <- symbolCdr
  ignored
  pure $ SymbolTree $ ((singleton car) :: String) <> cdr

parseChar :: Parser Tree
parseChar = do
  void $ char '\''
  c <- noneOf [ '\'' ]
  void $ char '\''
  ignored
  pure $ CharTree c

parseString :: Parser Tree
parseString = do
  void $ char '"'
  chars <- many (try escapedQuote <|> noneOf [ '\"' ]) <#> fromCharArray
  void $ char '"'
  ignored
  pure $ StringTree chars

parseInteger :: Parser Tree
parseInteger = do
  sign <- option "" $ string "-"
  number <- some digit <#> fromCharArray
  ignored
  result <- fromMaybe (fail "Invalid number") (map pure (fromString (sign <> number)))
  pure $ IntTree result

parseList :: Char -> Char -> Parser Tree
parseList open close = do
  void $ char open
  ignored
  args <- many (parseExpr unit)
  void $ char close
  ignored
  pure $ ApplyTree (fromFoldable args)

parseAtom :: Unit -> Parser Tree
parseAtom _ =
  parseSymbol
    <|> parseChar
    <|> parseString
    <|> parseInteger
    <|> parseList '(' ')'
    <|> parseList '[' ']'
    <|> parseList '{' '}'

parseExpr :: Unit -> Parser Tree
parseExpr _ = parseAtom unit

parseProgram :: String -> String -> Either ParseError (List Tree)
parseProgram _ input = runParser input (ignored *> many (parseAtom unit)) <#> fromFoldable

parseRepl :: String -> Either ParseError Tree
parseRepl input = runParser input (ignored *> (parseExpr unit))
