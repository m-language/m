module Eval where

import Prelude hiding (apply)

import Control.Alt ((<|>))
import Control.Monad.Except (Except, catchError, lift, mapExceptT, runExcept, throwError, withExceptT)
import Control.Monad.Reader (ask, local)
import Control.Monad.Trampoline (done)
import Data.Array as Array
import Data.BigInt (fromInt, fromNumber, toNumber)
import Data.Either (either)
import Data.Int (fromNumber) as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Lazy.NonEmpty as Nel
import Data.List.Lazy.Types (NonEmptyList)
import Data.Map (Map, lookup, union)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, d1, d2, reifyInt, toInt)
import Data.Vec (Vec)
import Data.Vec as Vec
import Effect.Exception (Error) as Js
import Effect.Exception (message, throw)
import Effect.Unsafe (unsafePerformEffect)
import Eval.Types (Env, Error(..), EvalResult(..), Process(..), Value(..), asDefine, asExpr, asInteger, asProcess, asString, runEvalResult)
import Foreign (F, Foreign, MultipleErrors, readArray, readBoolean, readNumber, readString, tagOf, typeOf, unsafeToForeign)
import Tree (Tree(..))
import Util (except)

foreign import callForeign :: forall m a. Array Foreign -> Foreign -> (Js.Error -> m a) -> (a -> m a) -> m Foreign
foreign import arity :: (forall a. Maybe a) -> (forall a. a -> Maybe a) -> Foreign -> Maybe Int

evalBlock :: Env -> List Tree -> EvalResult Env
evalBlock env trees = evalBlock' env false Set.empty Nil trees <#> \env' -> map (local $ \global -> union global env') env'

evalBlock' :: Env -> Boolean -> Set String -> List Tree -> List Tree -> EvalResult Env
evalBlock' env found errors Nil Nil = pure mempty
evalBlock' env true errors defer Nil = evalBlock' env false Set.empty Nil defer
evalBlock' env false errors defer Nil = throwError $ Undefined errors
evalBlock' env found errors defer (car : cdr) = do
  defs <- catchError (asDefine $ eval $ Tuple env car) \x -> case x of
    Undefined names -> evalBlock' env found (Set.union names errors) (car : defer) cdr
    Error string -> throwError $ Error string
  defs' <- local (\global -> union global env) $ evalBlock' (union defs env) true errors defer cdr
  pure $ union defs defs'

eval :: Tuple Env Tree -> EvalResult Value
eval (Tuple env (SymbolTree name)) = case lookup name env of
  Just value -> value
  Nothing -> do
    globals <- ask
    case lookup name globals of
      Just value -> value
      Nothing -> throwError $ Undefined $ Set.singleton name
eval (Tuple env (IntTree integer)) = pure $ IntValue integer
eval (Tuple env (CharTree char)) = pure $ CharValue char
eval (Tuple env (StringTree string)) = pure $ StringValue string
eval (Tuple env (ApplyTree fn arg)) = do
  f <- eval $ Tuple env fn
  apply env f $ List.singleton arg
eval (Tuple env (ListTree trees)) = do
  exprs <- traverse (asExpr <<< eval <<< Tuple env) trees
  pure $ Expr $ ListTree exprs

apply :: Env -> Value -> List Tree -> EvalResult Value
apply env f Nil = pure f
apply env (Macro f) args = f env args
apply env (Define defs) (arg : args) = do
  f <- eval $ Tuple (union env defs) arg
  apply env f args
apply env (Expr tree) (arg : args) = do
  evArg <- asExpr $ eval $ Tuple env arg
  apply env (Expr $ ApplyTree tree evArg) args
apply env x args = applyFn env x $ map eval $ map (Tuple env) args

applyFn :: Env -> Value -> List (EvalResult Value) -> EvalResult Value
applyFn _ f Nil = pure f
applyFn env (Function f) args = f env args
applyFn env (ProcessValue process) (map : args) = do
  evMap <- map
  let f = ProcessValue $ Do process \arg -> asProcess $ applyFn env evMap $ List.singleton $ pure arg
  applyFn env f args
applyFn env (ExternValue externFn) args = do
  foreignArgs <- traverse (\argument -> argument >>= unmarshall env) $ Array.fromFoldable args
  result <- callForeign foreignArgs externFn (message >>> Error >>> throwError) pure
  pure $ ExternValue $ result
applyFn _ x args = throwError $ Error $ "Expected function, found " <> show x

function :: Int -> (Env -> List (EvalResult Value) -> EvalResult Value) -> Value
function n f = Function fn
  where
    fn :: Env -> List (EvalResult Value) -> EvalResult Value
    fn env args
      | List.length args < n = 
          pure $ function (n - List.length args) \env' args' -> 
            applyFn env' (function n f) (args <> args')
      | List.length args > n = 
          applyFn env (function n f) (List.take n args) >>= \v ->
            applyFn env v (List.drop n args)
      | otherwise = f env args

functionN :: forall n. Nat n => n -> (Env -> Vec n (EvalResult Value) -> EvalResult Value) -> Value
functionN d fn = function (toInt d) func
  where 
    func :: Nat n => Env -> List (EvalResult Value) -> EvalResult Value
    func env = \args -> case Vec.fromArray $ Array.fromFoldable args of
      Just vec -> fn env vec
      Nothing -> throwError $ Error $ "Expected " <> show (toInt d) <> " arguments, got " <> (show $ List.length args)

macro :: Int -> (Env -> List Tree -> EvalResult Value) -> Value
macro n f = Macro fn
  where
    fn :: Env -> List Tree -> EvalResult Value
    fn env args
      | List.length args < n = 
          pure $ macro (n - List.length args) \env' args' -> 
            apply env' (macro n f) (args <> args')
      | List.length args > n = 
          apply env (macro n f) (List.take n args) >>= \v ->
            apply env v (List.drop n args)
      | otherwise = f env args

mkTrue :: Unit -> Value
mkTrue _ = functionN d2 \_ -> \vec -> Vec.head vec

mkFalse :: Unit -> Value
mkFalse _ = functionN d2 \_ -> \vec -> Vec.index vec d1

mkPair :: Value -> Value -> Value
mkPair fst snd = functionN d1 $ \env -> \fn -> do
  fnValue <- Vec.head fn
  applyFn env fnValue $ map pure $ fst : snd : Nil

data MarshallError = InvalidBigInt Number | Multiple MultipleErrors | Generic String
type MarshallResult a = Except (NonEmptyList MarshallError) a

liftMarshall :: forall a. MarshallResult a -> EvalResult a
liftMarshall = EvalResult <<< lift <<< mapExceptT (unwrap >>> done) <<< withExceptT (map (mError >>> show) >>> Array.fromFoldable >>> joinWith "," >>> Error)

liftResult :: forall a. F a -> MarshallResult a
liftResult = withExceptT $ Multiple >>> Nel.singleton

mError :: MarshallError -> Error
mError (Generic s) = Error s
mError (Multiple errors) = Error $ joinWith ", " $ Array.fromFoldable $ map show errors
mError (InvalidBigInt number) = Error $ "Expected foreign big integer, got " <> show number

mkObject :: Map String Value -> Value
mkObject kv = functionN d1 $ \env -> \keySymbol -> do
  key <- asString $ Vec.head keySymbol
  except (Error $ "Could not find key " <> show key <> "in object") $ lookup key kv

marshallInt :: Foreign -> MarshallResult Value
marshallInt fv = do
  n <- liftResult $ readNumber fv
  except (Nel.singleton $ InvalidBigInt n) $ fromNumber n <#> IntValue

marshallString :: Foreign -> MarshallResult Value
marshallString fv = liftResult $ readString fv <#> StringValue

marshallArray :: Foreign -> MarshallResult Value
marshallArray fv = do
  fa <- liftResult $ readArray fv
  mkArray fa
    where
      mkArray :: Array Foreign -> MarshallResult Value
      mkArray fa = traverse marshall fa <#> \elements -> 
        mkPair (IntValue $ fromInt $ Array.length elements) $ functionN d1 \_ -> \arrayIndex -> do
          n <- asInteger (Vec.head arrayIndex) <#> toNumber
          let result = Int.fromNumber n >>= Array.index elements
          except (Error $ "Expected number [0, " <> (show $ Array.length elements) <> "), found " <> show n) result

marshallBoolean :: Foreign -> MarshallResult Value
marshallBoolean fv = liftResult $ readBoolean fv <#> \fb -> if fb then mkTrue unit else mkFalse unit

marshallObject :: Foreign -> MarshallResult Value
marshallObject fv = do
  let isObject = typeOf fv == "object"
  throwError $ Nel.singleton $ Generic "Objects cannot be marshalled"

marshallFunction :: Foreign -> MarshallResult Value
marshallFunction fv = if typeOf fv /= "function"
  then throwError $ Nel.singleton $ Generic $ "Expected function, found " <> tagOf fv
  else do
    functionArity <- except (Nel.singleton $ Generic $ "Expected function, found " <> tagOf fv) $ arity Nothing Just fv
    pure $ reifyInt functionArity \n -> functionN n \env args -> do
      args' <- traverse (\arg -> arg >>= unmarshall env) $ Vec.toArray args
      result <- callForeign args' fv (message >>> Error >>> throwError) pure
      pure $ ExternValue $ result

marshall :: Foreign -> MarshallResult Value
marshall fv = marshallInt fv
          <|> marshallString fv
          <|> marshallArray fv  
          <|> marshallBoolean fv
          <|> marshallObject fv
          <|> marshallFunction fv

unsafeMarshall :: Foreign -> Value
unsafeMarshall fv = unsafePerformEffect $ either (map (mError >>> show) >>> Array.fromFoldable >>> joinWith ", " >>> throw) pure $ runExcept $ marshall fv

unmarshall :: Env -> Value -> EvalResult Foreign
unmarshall _ (ExternValue f) = pure f
unmarshall _ (IntValue i) = pure $ unsafeToForeign $ toNumber i
unmarshall _ (StringValue s) = pure $ unsafeToForeign s
unmarshall _ (CharValue c) = pure $ unsafeToForeign $ fromCharArray [c]
unmarshall foreignEnv p@(ProcessValue _) = unmarshall foreignEnv $ functionN d1 \env -> \args -> applyFn env p $ List.fromFoldable args
unmarshall foreignEnv (Function fn) = ask <#> \env' -> unsafeToForeign \(arg :: Foreign) ->
  let result = runEvalResult (fn foreignEnv (List.singleton $ pure $ ExternValue arg)) env' in
  let output = result >>= \returnValue -> runEvalResult (unmarshall foreignEnv returnValue) env' in
  unsafePerformEffect $ either (show >>> throw) (\v -> pure $ ExternValue v) output
unmarshall _ arg = throwError $ Error $ "Expected primitive value, found " <> show arg
