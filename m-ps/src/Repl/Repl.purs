module Repl where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (uncons)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Parse (incremental, replParser)
import Text.Parsing.Parser (ParseError, ParserT, hoistParserT, runParserT)
import Tree (Tree)
import Util (break)

type ReplInput m s = m s 

data ReplCommand = Eval Tree | Print Tree | Load FilePath

data ReplError = Parse ParseError | Generic String

instance showReplError :: Show ReplError where
  show (Parse e) = show e
  show (Generic s) = show s

class Monad m <= Repl m where
  error :: ReplError -> m Unit
  query :: String -> m String
  run :: ReplCommand -> m Unit

more :: forall m. Repl m => m String
more = query ".. "

input :: forall m. Repl m => m String
input = query "M> "

parse :: forall m. Repl m => String -> m (Maybe Tree)
parse line = do
  let parser = incremental more $ (hoistParserT (unwrap >>> pure) replParser :: ParserT String m (Maybe Tree))
  result <- runParserT line parser
  handle Nothing $ lmap Parse result

process :: forall m. Repl m => String -> m Unit
process line = case uncons line of
  Just ({ head: ':', tail: tail  }) -> do
    let (Tuple command expr) = break tail
    case command of
      "eval" -> do
        tree <- parse expr
        maybe (pure unit) (Eval >>> run) tree
      "load" -> do
        load expr
      "parse" -> do
        tree <- parse expr
        maybe (pure unit) (Print >>> run) tree
      _ -> do
        error $ Generic $ "Unknown command " <> command
  _ -> do
    tree <- parse line
    maybe (pure unit) (Eval >>> run) tree

handle :: forall m a. Repl m => a -> Either ReplError a -> m a
handle default result = either (\err -> error err *> pure default) pure result

load :: forall m. Repl m => String -> m Unit
load = run <<< Load

repl :: forall m. Repl m => m Unit
repl = do
  userInput <- input
  process userInput
  repl
