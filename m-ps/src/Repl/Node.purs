module Repl.Node where

import Prelude

import Control.Monad.Cont (ContT(..), lift, runContT)
import Control.Monad.Error.Class (class MonadThrow, throwError, try)
import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, modify, put)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map, union)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error) as Js
import Effect.Exception (message, throwException)
import Eval as Eval
import Eval.Types (Env, EvalResult, Process(..), Value(..), runEvalResult)
import Extern (externFile, loadExternal)
import IO (Input(..), io)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Process (stdout)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Node.Stream as Stream
import Parse (parseProgram)
import Partial.Unsafe (unsafePartial)
import Repl (class Repl, ReplCommand(..), ReplError(..))
import Repl as Repl
import Special (special)
import Tree (Tree)
import Util (doesFileExist, listFilesWithExtension, printEither, runDefault, throwEffect)

foreign import getCharImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

getChar :: Effect (Maybe Char)
getChar = getCharImpl Just Nothing <#> \m -> m >>= String.charAt 0

basicIO :: Input
basicIO = Input
    { getChar: getChar
    , putChar: putChar }
  where
    putChar = \char -> void $ Stream.writeString stdout UTF8 (String.singleton char) (pure unit)

newtype NodeRepl a = NodeRepl (ReaderT Interface (StateT Env (ExceptT Js.Error (ContT Unit Effect))) a)

derive instance newtypeNodeRepl :: Newtype (NodeRepl a) _
derive newtype instance functorNodeRepl :: Functor NodeRepl
derive newtype instance applyNodeRepl :: Apply NodeRepl
derive newtype instance bindNodeRepl :: Bind NodeRepl
derive newtype instance applicativeNodeRepl :: Applicative NodeRepl
derive newtype instance monadNodeRepl :: Monad NodeRepl
derive newtype instance monadEffectNodeRepl :: MonadEffect NodeRepl
derive newtype instance monadStateNodeRepl :: MonadState (Map String (EvalResult Value)) NodeRepl
derive newtype instance monadAskNodeRepl :: MonadAsk Interface NodeRepl
derive newtype instance monadReaderNodeRepl :: MonadReader Interface NodeRepl
derive newtype instance monadErrorNodeRepl :: MonadError Js.Error NodeRepl
derive newtype instance monadThrowNodeRepl :: MonadThrow Js.Error NodeRepl

tryEffect :: forall a. Effect a -> NodeRepl a
tryEffect eff = do
  result <- liftEffect $ try eff
  either throwError pure result

evalNodeRepl :: forall a. NodeRepl a -> Effect Unit
evalNodeRepl (NodeRepl n) = do
  let env = unsafePartial $ special <> io basicIO
  interface <- createConsoleInterface noCompletion
  runContT (runExceptT (evalStateT (runReaderT n interface) env)) $ either throwException (const $ pure unit)

instance replNodeRepl :: Repl Js.Error NodeRepl where
  error (Native err) = liftEffect $ log $ message err
  error err = liftEffect $ logShow err
  query prompt = do
    iface <- ask
    NodeRepl $ lift $ lift $ lift $ ContT \cont -> question prompt cont iface
  run (Eval tree) = do
    env <- get
    either logShow evaluateResult $ runEvalResult (Eval.eval $ Tuple mempty tree) env
  run (Print tree) = liftEffect $ logShow tree
  run (Load path) = loadFile path

runProcess :: Env -> Process -> MaybeT Effect (EvalResult Value)
runProcess env (Impure a) = lift $ pure <$> a
runProcess env (Do process fn) = do
  value <- runProcess env process
  proc <- pure $ value >>= fn
  process' <- printEither $ runEvalResult proc env
  runProcess env process'

evaluateResult :: Value -> NodeRepl Unit
evaluateResult value = case value of
  ProcessValue p ->  do
    env <- get
    tryEffect $ runDefault unit $ void $ runProcess env p
    log ""
    put env
  Define defs -> modify (union defs) *> pure unit
  _ -> liftEffect $ logShow value

loadFile :: FilePath -> NodeRepl Unit
loadFile path = do
  isFile <- tryEffect $ doesFileExist path
  Tuple extern trees <- if isFile
    then do
      tree <- parseFile path
      extern <- tryEffect $ maybe (pure $ Right mempty) (Array.singleton >>> loadExternal >>> runExceptT) $ externFile path
      pure $ Tuple extern tree
    else do
      tree <- parseFiles path
      jsFiles <- tryEffect $ listFilesWithExtension ".js" path
      extern <- tryEffect $ runExceptT $ loadExternal $ Array.fromFoldable jsFiles
      pure $ Tuple extern tree
  either logShow (\externEnv -> do
    env <- get
    let env' = union env externEnv
    evaluated <- tryEffect $ throwEffect $ runEvalResult (Eval.evalBlock mempty trees) env'
    put $ union evaluated env'
  ) extern
  
parseFile :: FilePath -> NodeRepl (List Tree)
parseFile name = do
  chars <- tryEffect $ readTextFile UTF8 name
  Repl.handle Nil $ lmap (show >>> Generic) $ parseProgram name chars

parseFiles :: FilePath -> NodeRepl (List Tree)
parseFiles path = do
  mfiles <- tryEffect $ listFilesWithExtension ".m" path
  parsed <- traverse parseFile mfiles
  pure $ List.concat parsed
