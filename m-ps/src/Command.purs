module Command where

import Control.Applicative
import Control.Comonad
import Control.Monad.Except
import Control.Monad.Maybe.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Writer.Trans
import Control.MonadZero
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Effect
import Effect.Console
import Eval
import Node.FS
import Node.FS.Sync
import Parse
import Prelude
import Tree
import Data.Map as Map
import Data.String (Pattern(..), split)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath)
import Node.Path as Path
import Text.Parsing.Parser (ParseError)

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

runCommand "load" rest env = runLoadCommand rest env

runCommand name _ env = log ("Unrecognized command " <> name) *> pure env

runParseCommand :: String -> Env -> Effect Env
runParseCommand rest env =
  runDefault env do
    tree <- printEither $ parseRepl rest
    lift $ log (show tree)
    pure env

runEvalCommand :: String -> Env -> Effect Env
runEvalCommand rest env =
  runDefault env do
    tree <- printEither $ parseRepl rest
    value <- printExcept $ runReaderT (eval (Tuple (Env Map.empty) tree)) env
    case value of
      ProcessValue p -> runProcess env p $> env
      Define defs -> pure $ unionEnv defs env
      x -> lift $ log (show value) $> env

runLoadParseCommand :: String -> Env -> Effect Env
runLoadParseCommand rest env =
  runDefault env do
    files <- lift $ parseFiles $ fromFoldable $ words rest
    trees <- printEither files
    for_ trees (lift <<< logShow)
    pure env

runLoadCommand :: String -> Env -> Effect Env
runLoadCommand rest env =
  runDefault env do
    files <- lift $ parseFiles $ fromFoldable $ words rest
    trees <- printEither files
    defs <- printExcept $ runReaderT (evalBlock (Env Map.empty) trees) env
    pure $ unionEnv defs env

parseFile :: String -> Effect (Either ParseError (List Tree))
parseFile name =
  doesDirectoryExist name
    >>= \x -> case x of
        true -> do
          names <- listDirectory name
          parseFiles $ fromFoldable $ map (\sub -> Path.concat [ name, sub ]) names
        false -> do
          chars <- readTextFile UTF8 name
          pure $ parseProgram name chars

parseFiles :: List String -> Effect (Either ParseError (List Tree))
parseFiles names = traverse parseFile names <#> (\f -> sequence f <#> concat)

printExcept :: forall a b. (Show a) => Except a b -> MaybeT Effect b
printExcept except = printEither $ extract $ runExceptT except

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
  process' <- printExcept $ runReaderT proc env
  runProcess env process'
