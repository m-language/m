module Eval.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trampoline (Trampoline)
import Data.Array as Array
import Data.BigInt (BigInt, toString)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.String (joinWith)
import Effect (Effect)
import Foreign (Foreign, typeOf)
import Tree (Tree(..))
import Data.Newtype (class Newtype)

data Process
  = Impure (Effect Value)
  | Do Process (Value -> EvalResult Process)

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

data Error
  = Error String
  | Undefined (Set String)

instance showValue :: Show Value where
  show (Function f) = "<function>"
  show (Macro f) = "<macro>"
  show (Expr t) = "'" <> show t
  show (CharValue c) = show c
  show (StringValue s) = show s
  show (IntValue i) = toString i
  show (Define (Env e)) = "{" <> (joinWith " " $ Array.fromFoldable $ Map.keys e) <> "}"
  show (ProcessValue p) = "<process>"
  show (ExternValue f) = "<extern " <> typeOf f <> ">"

instance showError :: Show Error where
  show (Error string) = "Error: " <> string
  show (Undefined ns) = "Undefined: " <> (joinWith " " $ Array.fromFoldable ns)

nil :: Value
nil = Expr $ ApplyTree Nil

newtype Env = Env (Map String Value)

derive instance envNewtype :: Newtype Env _

instance showEnv :: Show Env where
  show (Env m) = show m

instance envMonoid :: Monoid Env where
  mempty = Env mempty

instance envSemigroup :: Semigroup Env where
  append (Env a) (Env b) = Env $ append a b

insertEnv :: String -> Value -> Env -> Env
insertEnv name value (Env env) = Env $ Map.insert name value env

unionEnv :: Env -> Env -> Env
unionEnv (Env a) (Env b) = Env $ Map.union a b

lookupEnv :: String -> Env -> Maybe Value
lookupEnv name (Env env) = Map.lookup name env

type EvalResult = ReaderT Env (ExceptT Error Trampoline)

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