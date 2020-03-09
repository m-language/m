module Command where

import Control.Monad.Except (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trampoline (runTrampoline)
import Control.Monad.Writer.Trans (lift)
import Control.MonadZero (empty)
import Data.Either (Either(..))
import Data.List (List, concat, fromFoldable)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Eval (Env(..), EvalResult, Process(..), Value(..), eval, evalBlock, unionEnv)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (readTextFile, readdir, stat)
import Node.Path (FilePath)
import Node.Path as Path
import Parse (parseProgram, parseRepl)
import Prelude (class Show, bind, discard, map, pure, show, ($), ($>), (*>), (<#>), (<$>), (<<<), (<>), (>>=))
import Text.Parsing.Parser (ParseError)
import Tree (Tree)

listDirectory :: FilePath -> Effect (Array FilePath)
listDirectory = readdir

doesDirectoryExist :: FilePath -> Effect Boolean
doesDirectoryExist path = do
  stats <- stat path
  pure $ isDirectory stats

words :: String -> Array String
words = split (Pattern " ")

runCommand :: String -> String -> Env -> Effect Env
runCommand "parse" rest env = runParseCommand rest env
runCommand "eval" rest env = runEvalCommand rest env
runCommand "load-parse" rest env = runLoadParseCommand rest env
runCommand "load" rest env = runLoadCommand (words rest) env
runCommand name _ env = log ("Unrecognized command " <> name) *> pure env

runParseCommand :: String -> Env -> Effect Env
runParseCommand rest env = runDefault env do
  tree <- printEither $ parseRepl rest
  lift $ logShow tree
  pure env

runEvalCommand :: String -> Env -> Effect Env
runEvalCommand rest env = runDefault env do
  tree <- printEither $ parseRepl rest
  value <- printEither $ runTrampoline $ runExceptT $ runReaderT (eval (Tuple (Env Map.empty) tree)) env
  case value of
    ProcessValue p -> runProcess env p *> (lift $ log "") $> env
    Define defs -> pure $ unionEnv defs env
    x -> lift $ logShow value $> env

runLoadParseCommand :: String -> Env -> Effect Env
runLoadParseCommand rest env = runDefault env do
  files <- lift $ parseFiles $ fromFoldable $ words rest
  trees <- printEither files
  for_ trees $ lift <<< logShow
  pure env

runLoadCommand :: Array String -> Env -> Effect Env
runLoadCommand paths env = runDefault env do
  files <- lift $ parseFiles $ fromFoldable paths
  trees <- printEither files
  defs <- printEither $ runTrampoline $ runExceptT $ runReaderT (evalBlock (Env Map.empty) trees) env
  pure $ unionEnv defs env

parseFile :: String -> Effect (Either ParseError (List Tree))
parseFile name = doesDirectoryExist name >>= \x -> if x
  then do
    names <- listDirectory name
    parseFiles $ fromFoldable $ map (\sub -> Path.concat [ name, sub ]) names
  else do
    chars <- readTextFile UTF8 name
    pure $ parseProgram name chars

parseFiles :: List String -> Effect (Either ParseError (List Tree))
parseFiles names = traverse parseFile names <#> \f -> sequence f <#> concat

printEither :: forall a b. (Show a) => Either a b -> MaybeT Effect b
printEither error = case error of
  Left e -> lift (log (show e)) *> empty
  Right b -> pure b

runDefault :: forall a. a -> MaybeT Effect a -> Effect a
runDefault a maybeT = runMaybeT maybeT <#> fromMaybe a

runProcess :: Env -> Process -> MaybeT Effect (EvalResult Value)
runProcess env (Impure a) = lift $ pure <$> a
runProcess env (Do process fn) = do
  value <- runProcess env process
  proc <- pure $ value >>= fn
  process' <- printEither $ runTrampoline $ runExceptT $ runReaderT proc env
  runProcess env process'
