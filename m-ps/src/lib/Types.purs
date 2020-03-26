module Eval.Types where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Data.Array as Array
import Data.BigInt (BigInt, toString)
import Data.Either (Either)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.String (joinWith)
import Effect (Effect)
import Foreign (Foreign, typeOf)
import Tree (Tree)

data Process
  = Impure (Effect Value)
  | Do Process (Value -> EvalResult Process)

data Error
  = Error String
  | Undefined (Set String)

data Value
  = Function (Env -> List (EvalResult Value) -> EvalResult Value)
  | Macro (Env -> List Tree -> EvalResult Value)
  | Define Env
  | Expr Tree
  | CharValue Char
  | StringValue String
  | IntValue BigInt
  | ProcessValue Process
  | ExternValue Foreign

instance showValue :: Show Value where
  show (Function f) = "<function>"
  show (Macro f) = "<macro>"
  show (Expr t) = "'" <> show t
  show (CharValue c) = show c
  show (StringValue s) = show s
  show (IntValue i) = toString i
  show (Define e) = "{" <> (joinWith " " $ Array.fromFoldable $ Map.keys e) <> "}"
  show (ProcessValue p) = "<process>"
  show (ExternValue f) = "<extern " <> typeOf f <> ">"

derive instance eqError :: Eq Error

instance showError :: Show Error where
  show (Error string) = "Error: " <> string
  show (Undefined ns) = "Undefined: " <> (joinWith " " $ Array.fromFoldable ns)

nil :: Value
nil = Define mempty

type Env = Map String (EvalResult Value)

newtype EvalResult a = EvalResult (ReaderT Env (ExceptT Error Trampoline) a)

derive newtype instance functorEvalResult :: Functor EvalResult
derive newtype instance applyEvalResult :: Apply EvalResult
derive newtype instance bindEvalResult :: Bind EvalResult
derive newtype instance applicativeEvalResult :: Applicative EvalResult
derive newtype instance monadEvalResult :: Monad EvalResult
derive newtype instance monadThrowEvalResult :: MonadThrow Error EvalResult
derive newtype instance monadErrorEvalResult :: MonadError Error EvalResult
derive newtype instance monadAskEvalResult :: MonadAsk (Map String (EvalResult Value)) EvalResult
derive newtype instance monadReaderEvalResult :: MonadReader (Map String (EvalResult Value)) EvalResult

runEvalResult :: forall a. EvalResult a -> Env -> Either Error a
runEvalResult (EvalResult r) env = runTrampoline $ runExceptT $ runReaderT r env

asDefine :: EvalResult Value -> EvalResult Env
asDefine tree = tree >>= \x -> case x of
  (Define defs) -> pure defs
  err -> throwError $ Error $ "Expected definition, found " <> show err

asExpr :: EvalResult Value -> EvalResult Tree
asExpr tree = tree >>= \x -> case x of
  (Expr t) -> pure t
  err -> throwError $ Error $ "Expected expression, found " <> show err

asChar :: EvalResult Value -> EvalResult Char
asChar tree = tree >>= \x -> case x of
  (CharValue char) -> pure char
  err -> throwError $ Error $ "Expected character, found " <> show err

asString :: EvalResult Value -> EvalResult String
asString tree = tree >>= \x -> case x of
  (StringValue string) -> pure string
  err -> throwError $ Error $ "Expected string, found " <> show err

asInteger :: EvalResult Value -> EvalResult BigInt
asInteger tree = tree >>= \x -> case x of
  (IntValue i) -> pure i
  err -> throwError $ Error $ "Expected integer, found " <> show err

asProcess :: EvalResult Value -> EvalResult Process
asProcess tree = tree >>= \x -> case x of
  (ProcessValue p) -> pure p
  err -> throwError $ Error $ "Expected process, found " <> show err

asExtern :: EvalResult Value -> EvalResult Foreign
asExtern tree = tree >>= \x -> case x of
  (ExternValue f) -> pure f
  err -> throwError $ Error $ "Expected extern, found " <> show err