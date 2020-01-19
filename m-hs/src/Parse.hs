module Parse where

import           Tree
import           Text.ParserCombinators.Parsec
import           System.Environment
import           Control.Monad

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
opChar = oneOf "!$%&*+|:<=>?@^~'=-"

symbolChar :: Parser Char
symbolChar = alphaNum

symbolCdr :: Parser String
symbolCdr = many (symbolChar <|> opChar)

escapedQuote :: Parser Char
escapedQuote = do
    quote <- string "\"\""
    return $ last quote

parseInlineSymbol :: Parser String
parseInlineSymbol = do
    car <- symbolChar
    cdr <- symbolCdr
    return $ car : cdr

parseLiteralSymbol :: Parser String
parseLiteralSymbol = do
    char '"'
    chars <- many (try escapedQuote <|> noneOf "\"")
    char '"'
    return chars

parseSymbol :: Parser Tree
parseSymbol = do
    symbol <- sepBy1 (parseInlineSymbol <|> parseLiteralSymbol) (char '/')
    ignored
    return $ Symbol symbol

parseList :: Char -> Char -> Parser Tree
parseList open close = do
    char open
    ignored
    fn <- parseExpr
    args <- many parseExpr
    char close
    ignored
    return $ Apply fn args

parseAtom :: Parser Tree
parseAtom =
    parseSymbol
        <|> parseList '(' ')'
        <|> parseList '[' ']'
        <|> parseList '{' '}'

parseExpr :: Parser Tree
parseExpr = parseAtom

parseProgram :: String -> String -> Either ParseError [Tree]
parseProgram = parse (ignored >> many parseAtom)

parseRepl :: String -> Either ParseError Tree
parseRepl = parse (ignored >> parseExpr) "<stdin>"
