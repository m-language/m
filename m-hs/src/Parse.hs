module Parse where

import           Tree
import           Text.ParserCombinators.Parsec
import           System.Environment
import           Control.Monad
import           Data.Functor

lineCommentTail :: Parser ()
lineCommentTail = void $ many $ noneOf "\n"

blockCommentTail :: Parser ()
blockCommentTail = do
    char '('
    many (blockCommentTail <|> void (noneOf "()"))
    char ')'
    return ()

comment :: Parser ()
comment = char '#' >> (blockCommentTail <|> lineCommentTail)

ignored :: Parser ()
ignored = skipMany (comment <|> void space)

opChar :: Parser Char
opChar = oneOf "~`!@$%^&*-=+\\|;:<>,.?/"

symbolChar :: Parser Char
symbolChar = letter

symbolCdr :: Parser String
symbolCdr = many (symbolChar <|> digit <|> opChar)

escapedQuote :: Parser Char
escapedQuote = do
    quote <- string "\"\""
    return $ last quote

parseSymbol :: Parser Tree
parseSymbol = do
    car <- symbolChar
    cdr <- symbolCdr
    ignored
    return $ Symbol $ car : cdr

parseChar :: Parser Tree
parseChar = do
    char '\''
    c <- noneOf "'"
    char '\''
    ignored
    return $ CharTree c

parseString :: Parser Tree
parseString = do
    char '"'
    chars <- many (try escapedQuote <|> noneOf "\"")
    char '"'
    ignored
    return $ StringTree chars

parseInteger :: Parser Tree
parseInteger = do
    sign   <- option "" $ string "-"
    number <- many1 digit
    ignored
    return $ IntTree $ read (sign ++ number)

parseList :: Char -> Char -> Parser Tree
parseList open close = do
    char open
    ignored
    args <- many parseExpr
    char close
    ignored
    return $ Apply args

parseAtom :: Parser Tree
parseAtom =
    parseSymbol
        <|> parseChar
        <|> parseString
        <|> parseInteger
        <|> parseList '(' ')'
        <|> parseList '[' ']'
        <|> parseList '{' '}'

parseExpr :: Parser Tree
parseExpr = parseAtom

parseProgram :: String -> String -> Either ParseError [Tree]
parseProgram = parse (ignored >> many parseAtom)

parseRepl :: String -> Either ParseError Tree
parseRepl = parse (ignored >> parseExpr) "<stdin>"
