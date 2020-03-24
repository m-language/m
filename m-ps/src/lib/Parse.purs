module Parse where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (get, lift, put)
import Data.Array (many, some)
import Data.Bifunctor (lmap)
import Data.BigInt (fromString)
import Data.Either (Either)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Text.Parsing.Parser (ParseError, fail, runParser)
import Text.Parsing.Parser (ParseState(..), Parser, ParserT) as P
import Text.Parsing.Parser.Combinators (option, skipMany, try)
import Text.Parsing.Parser.String (class StringLike, char, eof, noneOf, null, oneOf, string)
import Text.Parsing.Parser.Token (digit, letter, space)
import Tree (Tree(..))

type Parser a = P.Parser String a
data ParsingError = ParsingError String ParseError

instance showParsingError :: Show ParsingError where
  show (ParsingError file error) = "(" <> file <> " " <> show error <> ")"

incremental :: forall m s a. StringLike s => Semigroup s => Monad m => m s -> P.ParserT s m a -> P.ParserT s m a
incremental more incrementalParser = do
  (P.ParseState previous position _) <- get
  catchError (try incrementalParser) \e -> do
    (P.ParseState leftOver _ consumed) <- get
    if null leftOver
      then do
        extra <- lift $ more
        put $ P.ParseState (previous <> extra) position consumed
        incremental more incrementalParser
      else
        throwError e

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
ignored = skipMany $ comment <|> void space

opChar :: Parser Char
opChar = oneOf $ toCharArray "~'`!@$%^&*-=+\\|;:<>,.?/_"

symbolChar :: Parser Char
symbolChar = letter <|> opChar

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
  pure $ SymbolTree $ singleton car <> cdr

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
  args <- many (parseAtom unit)
  void $ char close
  ignored
  pure $ ApplyTree (fromFoldable args)

parseAtom :: Unit -> Parser Tree
parseAtom more =
  parseSymbol
    <|> parseChar
    <|> parseString
    <|> parseInteger
    <|> parseList '(' ')'
    <|> parseList '[' ']'
    <|> parseList '{' '}'

parseAll :: Parser (List Tree)
parseAll = (((ignored *> many (parseAtom unit)) <#> fromFoldable) <* ignored <* eof)

replParser :: Parser (Maybe Tree)
replParser = (ignored *> (option Nothing $ parseAtom unit <#> Just) <* ignored <* eof)

parseProgram :: String -> String -> Either ParsingError (List Tree)
parseProgram file input = lmap (ParsingError file) $ runParser input parseAll

parseRepl :: String -> Either ParseError (Maybe Tree)
parseRepl input = runParser input replParser
