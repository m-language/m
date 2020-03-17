module Repl.Node where

import Prelude

import Control.Monad.Cont (ContT(..), lift, runContT)
import Control.Monad.Except (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, modify, put)
import Control.Monad.Trampoline (runTrampoline)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Eval as Eval
import Eval.Types (Env, EvalResult, Process(..), Value(..), unionEnv)
import Extern (loadExternal)
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
import Util (listFilesWithExtension, printEither, runDefault, throwEffect)

foreign import readInputCharImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

getChar :: Effect (Maybe Char)
getChar = readInputCharImpl Just Nothing <#> \m -> m >>= String.charAt 0

basicIO :: Input
basicIO = Input
    { getChar: getChar
    , putChar: putChar }
  where
    putChar = \char -> void $ Stream.writeString stdout UTF8 (String.singleton char) (pure unit)

newtype NodeRepl a = NodeRepl (ReaderT Interface (StateT Env (ContT Unit Effect)) a)

derive instance newtypeNodeRepl :: Newtype (NodeRepl a) _
derive newtype instance functorNodeRepl :: Functor NodeRepl
derive newtype instance applyNodeRepl :: Apply NodeRepl
derive newtype instance bindNodeRepl :: Bind NodeRepl
derive newtype instance applicativeNodeRepl :: Applicative NodeRepl
derive newtype instance monadNodeRepl :: Monad NodeRepl
derive newtype instance monadEffectNodeRepl :: MonadEffect NodeRepl
derive newtype instance monadStateNodeRepl :: MonadState Env NodeRepl

evalNodeRepl :: forall a. NodeRepl a -> Effect Unit
evalNodeRepl (NodeRepl n) = do
  let env = unsafePartial (special <> io basicIO)
  interface <- createConsoleInterface noCompletion
  runContT (evalStateT (runReaderT n interface) env) $ const $ pure unit

instance replNodeRepl :: Repl NodeRepl where
  error err = NodeRepl $ liftEffect $ logShow err
  query prompt = NodeRepl $ do
    iface <- ask
    lift $ lift $ ContT \cont -> question prompt cont iface
  run (Eval tree) = NodeRepl $ do
    env <- get
    either logShow (unwrap <<< evaluateResult) $ runTrampoline $ runExceptT $ runReaderT (Eval.eval $ Tuple mempty tree) env
  run (Print tree) = NodeRepl $ liftEffect $ logShow tree
  run (Load path) = loadFile path

      
runProcess :: Env -> Process -> MaybeT Effect (EvalResult Value)
runProcess env (Impure a) = lift $ pure <$> a
runProcess env (Do process fn) = do
  value <- runProcess env process
  proc <- pure $ value >>= fn
  process' <- printEither $ runTrampoline $ runExceptT $ runReaderT proc env
  runProcess env process'

evaluateResult :: Value -> NodeRepl Unit
evaluateResult value = NodeRepl $ case value of
  ProcessValue p ->  do
    env <- get
    liftEffect $ runDefault unit $ void $ runProcess env p
    log ""
    put env
  Define defs -> modify (unionEnv defs) *> pure unit
  _ -> liftEffect $ logShow value

loadFile :: FilePath -> NodeRepl Unit
loadFile path = do
  trees <- parseFiles path
  jsFiles <- liftEffect $ listFilesWithExtension ".js" path
  extern <- liftEffect $ runExceptT $ loadExternal $ Array.fromFoldable jsFiles
  either logShow (\externEnv -> do
    env <- get
    let env' = unionEnv env externEnv
    evaluated <- liftEffect $ throwEffect $ runTrampoline $ runExceptT $ runReaderT (Eval.evalBlock mempty trees) env'
    put $ unionEnv evaluated env'
  ) extern

parseFile :: FilePath -> NodeRepl (List Tree)
parseFile name = do
  chars <- liftEffect $ readTextFile UTF8 name
  Repl.handle Nil $ lmap (show >>> Generic) $ parseProgram name chars

parseFiles :: FilePath -> NodeRepl (List Tree)
parseFiles path = do
  mfiles <- liftEffect $ listFilesWithExtension ".m" path
  parsed <- traverse parseFile mfiles
  pure $ List.concat parsed
