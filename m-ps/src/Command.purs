module Command where

import Prelude

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, except, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (StateT, get, mapStateT, put)
import Control.Monad.Trampoline (runTrampoline)
import Control.Monad.Writer.Trans (lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Eval (eval, evalBlock)
import Eval.Types (Env, Error, EvalResult, Process(..), Value(..), unionEnv)
import Extern (ExternError, loadExternal)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Parse (ParsingError(..), incremental, parseProgram, replParser)
import Text.Parsing.Parser (hoistParserT, runParserT)
import Tree (Tree)
import Util (listFilesWithExtension, printEither, runDefault, throwEffect, words)

data CommandError = Parse ParsingError | RuntimeError Error | ExternError ExternError

type CommandT m a = StateT Env m a
type Command a = CommandT Identity a

type InputCont = ContT Unit Effect String

instance commandErrorShow :: Show CommandError where
  show (Parse p) = show p
  show (RuntimeError e) = show e
  show (ExternError e) = show e


runCommand :: String -> String -> InputCont -> CommandT (ContT Unit Effect) Unit
runCommand "parse" rest more = runParseCommand rest more
runCommand "eval" rest more = runEvalCommand rest more
runCommand "load-parse" rest _ = mapStateT liftEffect $ runLoadParseCommand rest
runCommand "load" rest _ = mapStateT liftEffect $ runLoadCommand (List.fromFoldable $ words rest)
runCommand name _ _ = liftEffect $ log ("Unrecognized command " <> name)

runParseCommand :: String -> InputCont -> CommandT (ContT Unit Effect) Unit
runParseCommand rest more = runDefault unit do
  parsed <- lift $ lift $ parseWithContinuation rest more
  parsedTree <- printEither parsed
  tree <- MaybeT $ pure parsedTree
  liftEffect $ logShow tree

runEvalCommand :: String -> InputCont -> StateT Env (ContT Unit Effect) Unit
runEvalCommand rest more = runDefault unit do
  parsed <- lift $ lift $ parseWithContinuation rest more
  parsedTree <- printEither parsed
  tree <- MaybeT $ pure parsedTree
  env <- get
  value <- printEither $ runTrampoline $ runExceptT $ runReaderT (eval (Tuple mempty tree)) env
  lift $ mapStateT liftEffect $ evaluateResult value

runLoadParseCommand :: String -> CommandT Effect Unit
runLoadParseCommand rest = runDefault unit do
  files <- liftEffect $ runExceptT $ parseFiles $ List.fromFoldable $ words rest
  trees <- printEither files
  for_ trees $ liftEffect <<< logShow

runLoadCommand :: List String -> CommandT Effect Unit
runLoadCommand paths = runDefault unit do
  files <- liftEffect $ runExceptT $ parseFiles paths
  jsFiles <- liftEffect $ traverse (listFilesWithExtension ".js") paths <#> List.concat
  extern <- liftEffect $ runExceptT $ loadExternal $ Array.fromFoldable jsFiles
  externEnv <- printEither extern
  trees <- printEither files
  env <- get
  let env' = unionEnv env externEnv
  evaluated <- liftEffect $ throwEffect $ runTrampoline $ runExceptT $ runReaderT (evalBlock mempty trees) env'
  put $ unionEnv evaluated env'

parseFile :: String -> ExceptT CommandError Effect (List Tree)
parseFile name = do
  chars <- lift $ readTextFile UTF8 name
  except $ lmap Parse $ parseProgram name chars

parseFiles :: List String -> ExceptT CommandError Effect (List Tree)
parseFiles paths = do
  mfiles <- lift $ traverse (listFilesWithExtension ".m") paths <#> List.concat
  parsed <- traverse (\path -> parseFile path) mfiles
  pure $ List.concat parsed

runProcess :: Env -> Process -> MaybeT Effect (EvalResult Value)
runProcess env (Impure a) = lift $ pure <$> a
runProcess env (Do process fn) = do
  value <- runProcess env process
  proc <- pure $ value >>= fn
  process' <- printEither $ runTrampoline $ runExceptT $ runReaderT proc env
  runProcess env process'

parseWithContinuation :: String -> ContT Unit Effect String -> ContT Unit Effect (Either ParsingError (Maybe Tree))
parseWithContinuation input more = map (lmap (ParsingError "<stdin>")) $ runParserT input $ incremental more $ hoistParserT (unwrap >>> pure) replParser

evaluateResult :: Value -> StateT Env Effect Unit
evaluateResult value = get >>= \env -> case value of
  ProcessValue p -> do
    lift $ runDefault unit $ void $ runProcess env p
    liftEffect $ log ""
    put env
  Define defs -> put $ unionEnv defs env
  _ -> lift $ logShow value

parseAndEvaluate :: String -> ContT Unit Effect String -> StateT Env (ContT Unit Effect) Unit
parseAndEvaluate input moreInput = do
  completely <- lift $ parseWithContinuation input moreInput
  mapStateT liftEffect $ runDefault unit do
    env <- get
    parsedTree <- printEither completely
    tree <- MaybeT $ pure parsedTree
    resultValue <- printEither $ runTrampoline $ runExceptT $ runReaderT (eval (Tuple mempty tree)) env
    lift $ evaluateResult resultValue
