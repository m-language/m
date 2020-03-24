module Repl where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (drop)
import Data.String.CodeUnits (uncons)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Parse (incremental, replParser)
import Text.Parsing.Parser (ParseError, ParserT, hoistParserT, runParserT)
import Tree (Tree)
import Util (break)

type ReplInput m s = m s 

data ReplCommand = Eval Tree | Print Tree | Load FilePath

data ReplError e = Parse ParseError | Generic String | Native e

instance showReplError :: Show e => Show (ReplError e) where
  show (Parse e) = show e
  show (Generic s) = s
  show (Native er) = show er

class MonadError e m <= Repl e m | m -> e where
  error :: ReplError e -> m Unit
  query :: String -> m String
  run :: ReplCommand -> m Unit

more :: forall m e. Repl e m => m String
more = query ".. "

input :: forall m e. Repl e m => m String
input = query "M> "

parse :: forall m e. Repl e m => String -> m (Maybe Tree)
parse line = do
  let parser = incremental more $ (hoistParserT (unwrap >>> pure) replParser :: ParserT String m (Maybe Tree))
  result <- runParserT line parser
  handle Nothing $ lmap Parse result

runCommand :: forall e m. Repl e m => ReplCommand -> m Unit
runCommand command = catchError (run command) (\e -> error $ Native e)

process :: forall m e. Repl e m => String -> m Unit
process line = case uncons line of
  Just ({ head: ':', tail: tail  }) -> do
    let (Tuple command expr) = break tail
    case command of
      "eval" -> do
        tree <- parse expr
        maybe (pure unit) (Eval >>> run) tree
      "load" -> do
        load $ drop 1 expr
      "parse" -> do
        tree <- parse expr
        maybe (pure unit) (Print >>> run) tree
      _ -> do
        error $ Generic $ "Unknown command " <> command
  _ -> do
    tree <- parse line
    maybe (pure unit) (Eval >>> run) tree

handle :: forall m a e. Repl e m => a -> Either (ReplError e) a -> m a
handle default result = either (\err -> error err *> pure default) pure result

load :: forall m e. Repl e m => String -> m Unit
load = runCommand <<< Load

repl :: forall m e. Repl e m => m Unit
repl = do
  userInput <- input
  process userInput
  repl