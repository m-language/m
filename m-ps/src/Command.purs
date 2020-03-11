module Command where

import Prelude

import Control.Monad.Except (ExceptT, except, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trampoline (runTrampoline)
import Control.Monad.Writer.Trans (lift)
import Control.MonadZero (empty)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (throw)
import Eval (eval, evalBlock)
import Eval.Types (Env, Error, EvalResult, Process(..), Value(..), unionEnv)
import Extern (ExternError, loadExternal)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isDirectory, isFile)
import Node.FS.Sync (readTextFile, readdir, stat)
import Node.Path (FilePath)
import Node.Path as Path
import Parse (ParsingError, parseProgram, parseRepl)
import Tree (Tree)

data CommandError = Parse ParsingError | RuntimeError Error | ExternError ExternError

instance commandErrorShow :: Show CommandError where
  show (Parse p) = show p
  show (RuntimeError e) = show e
  show (ExternError e) = show e

listDirectory :: FilePath -> Effect (List FilePath)
listDirectory root = readdir root <#> (\paths -> List.fromFoldable $ paths <#> \path -> Path.concat [root, path]) >>= \paths -> do
  files <- List.filterM doesFileExist paths
  directories <- List.filterM doesDirectoryExist paths
  subFiles <- traverse listDirectory directories <#> List.concat
  pure $ List.concat $ List.fromFoldable [files, subFiles]

listFilesWithExtension :: String -> FilePath -> Effect (List FilePath)
listFilesWithExtension end root = (listDirectory root <#> List.filter \file -> Path.extname file == end) >>= (traverse (Path.resolve []))

externFile :: FilePath -> Maybe FilePath
externFile file = 
  if Path.extname file /= "m"
  then Nothing
  else do
    let dir = Path.dirname file
    let base = Path.basenameWithoutExt file ".m"
    pure $ Path.concat [dir, base <> ".js"]

doesDirectoryExist :: FilePath -> Effect Boolean
doesDirectoryExist path = do
  stats <- stat path
  pure $ isDirectory stats

doesFileExist :: FilePath -> Effect Boolean
doesFileExist path = do
  stats <- stat path
  pure $ isFile stats

words :: String -> Array String
words = split (Pattern " ")

runCommand :: String -> String -> Env -> Effect Env
runCommand "parse" rest env = runParseCommand rest env
runCommand "eval" rest env = runEvalCommand rest env
runCommand "load-parse" rest env = runLoadParseCommand rest env
runCommand "load" rest env = runLoadCommand (List.fromFoldable $ words rest) env
runCommand name _ env = log ("Unrecognized command " <> name) *> pure env

runParseCommand :: String -> Env -> Effect Env
runParseCommand rest env = runDefault env do
  tree <- printEither $ parseRepl rest
  lift $ logShow tree
  pure env

runEvalCommand :: String -> Env -> Effect Env
runEvalCommand rest env = runDefault env do
  tree <- printEither $ parseRepl rest
  value <- printEither $ runTrampoline $ runExceptT $ runReaderT (eval (Tuple mempty tree)) env
  case value of
    ProcessValue p -> runProcess env p *> (lift $ log "") $> env
    Define defs -> pure $ unionEnv defs env
    x -> lift $ logShow value $> env

runLoadParseCommand :: String -> Env -> Effect Env
runLoadParseCommand rest env = runDefault env do
  files <- lift $ runExceptT $ parseFiles $ List.fromFoldable $ words rest
  trees <- printEither files
  for_ trees $ lift <<< logShow
  pure env

runLoadCommand :: List String -> Env -> Effect Env
runLoadCommand paths env = runDefault env do
  files <- lift $ runExceptT $ parseFiles paths
  jsFiles <- lift $ traverse (listFilesWithExtension ".js") paths <#> List.concat
  extern <- lift $ runExceptT $ loadExternal $ Array.fromFoldable jsFiles
  externEnv <- printEither extern
  trees <- printEither files
  let env' = unionEnv env externEnv
  evaluated <- lift $ throwEffect $ runTrampoline $ runExceptT $ runReaderT (evalBlock mempty trees) env'
  pure $ unionEnv evaluated env'

parseFile :: String -> ExceptT CommandError Effect (List Tree)
parseFile name = do
  chars <- lift $ readTextFile UTF8 name
  except $ lmap Parse $ parseProgram name chars

parseFiles :: List String -> ExceptT CommandError Effect (List Tree)
parseFiles paths = do
  mfiles <- lift $ traverse (listFilesWithExtension ".m") paths <#> List.concat
  parsed <- traverse parseFile mfiles
  pure $ List.concat parsed

printEither :: forall a b. (Show a) => Either a b -> MaybeT Effect b
printEither error = case error of
  Left e -> lift (logShow e) *> empty
  Right b -> pure b

throwEffect :: forall e a. (Show e) => Either e a -> Effect a
throwEffect (Left e) = throw $ show e
throwEffect (Right a) = pure a 

runDefault :: forall a. a -> MaybeT Effect a -> Effect a
runDefault a maybeT = runMaybeT maybeT <#> fromMaybe a

runProcess :: Env -> Process -> MaybeT Effect (EvalResult Value)
runProcess env (Impure a) = lift $ pure <$> a
runProcess env (Do process fn) = do
  value <- runProcess env process
  proc <- pure $ value >>= fn
  process' <- printEither $ runTrampoline $ runExceptT $ runReaderT proc env
  runProcess env process'
