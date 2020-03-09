module Special where

import Control.Monad.Except (throwError)
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.List ((:), List(..), drop, length, take)
import Data.List as List
import Data.Map as Map
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..), curry)
import Eval (Env(..), Error(..), EvalResult, Process(..), Value(..), apply, applyFn, asChar, asExpr, asInteger, asString, eval, evalBlock, insertEnv)
import Prelude (bind, otherwise, pure, show, ($), (*), (+), (-), (/), (<), (<#>), (<$>), (<<<), (<>), (==), (>), (>>=), (>>>))
import Tree (Tree(..))

function :: Int -> (Env -> List (EvalResult Value) -> EvalResult Value) -> Value
function n f = Function fn
  where
    fn :: Env -> List (EvalResult Value) -> EvalResult Value
    fn env args
      | length args < n = 
          pure $ function (n - length args) \env' args' -> 
            applyFn env' (function n f) (args <> args')
      | length args > n = 
          applyFn env (function n f) (take n args) >>= \v ->
            applyFn env v (drop n args)
      | otherwise = f env args

macro :: Int -> (Env -> List Tree -> EvalResult Value) -> Value
macro n f = Macro fn
  where
    fn :: Env -> List Tree -> EvalResult Value
    fn env args
      | length args < n = 
          pure $ macro (n - length args) \env' args' -> 
            apply env' (macro n f) (args <> args')
      | length args > n = 
          apply env (macro n f) (take n args) >>= \v ->
            apply env v (drop n args)
      | otherwise = f env args

special :: Partial => Env
special = Env $ Map.fromFoldable
    [ Tuple "fn" $ macro 2 fn'
    , Tuple "fm" $ macro 2 fm'
    , Tuple "def"  $ macro 2 def'
    , Tuple "block" $ macro 1 block'
    , Tuple "quote" $ macro 1 quote'
    , Tuple "error" $ function 1 error'
    , Tuple "impure" $ function 1 impure' 
    , Tuple "expr-ops" expr'
    , Tuple "int-ops" int'
    , Tuple "char-ops" char'
    , Tuple "string-ops" string'
    ]

getNames :: Tree -> EvalResult (List String)
getNames (SymbolTree name) = pure $ name : Nil
getNames (ApplyTree args) = names args
  where
    names :: List Tree -> EvalResult (List String)
    names Nil = pure Nil
    names ((SymbolTree name) : cdr) = names cdr <#> ((:) name)
    names expr = throwError $ Error $ "Expected symbol, found " <> show expr
getNames expr = throwError $ Error $ "Expected symbol, found " <> show expr

fn' :: Partial => Env -> List Tree -> EvalResult Value
fn' closure (argsNames : expr : Nil) = do
  names <- getNames argsNames
  pure $ function (length names) \env args ->
      fnApply closure names args expr

fnApply :: Partial => Env -> List String -> List (EvalResult Value) -> Tree -> EvalResult Value
fnApply closure Nil Nil tree = eval $ Tuple closure tree
fnApply closure (name : names) (arg : args) tree = do
  evArg <- arg
  fnApply (insertEnv name evArg closure) names args tree

fm' :: Partial => Env -> List Tree -> EvalResult Value
fm' closure (argNames : expr : Nil) = do
  names <- getNames argNames
  pure $ macro (length names) \env args ->
      fmApply env closure names args expr

fmApply :: Partial => Env -> Env -> List String -> List Tree -> Tree -> EvalResult Value
fmApply env closure Nil Nil tree = asExpr (eval (Tuple closure tree)) >>= curry eval env
fmApply env closure (name : names) (arg : args) tree = 
  fmApply env (insertEnv name (Expr arg) closure) names args tree

def' :: Partial => Env -> List Tree -> EvalResult Value
def' env (names : expr : Nil) = case names of
  SymbolTree name -> do
    evExpr <- eval $ Tuple env expr
    pure $ Define $ Env $ Map.singleton name evExpr
  x -> throwError $ Error $ "Expected symbol, found " <> show x

block' :: Partial => Env -> List Tree -> EvalResult Value
block' env (exprs : Nil) = case exprs of
  SymbolTree name -> eval $ Tuple env $ SymbolTree name
  ApplyTree args -> Define <$> evalBlock env args

quote' :: Partial => Env -> List Tree -> EvalResult Value
quote' env (tree : Nil) = pure $ Expr tree

error' :: Partial => Env -> List (EvalResult Value) -> EvalResult Value
error' env (expr : Nil) = asString expr >>= (throwError <<< Error)

impure' :: Partial => Env -> List (EvalResult Value) -> EvalResult Value
impure' env (val : Nil) = ProcessValue <<< Impure <<< pure <$> val

expr' :: Partial => Value
expr' = Define $ Env $ Map.fromFoldable [ Tuple "case" $ function 4 case' ]
  where
    case' env (expr : sym : nil : ap : Nil) = asExpr expr >>= doCase
      where
        doCase (ApplyTree Nil) = nil
        doCase (ApplyTree (fn : args)) = do
          evAp <- ap
          applyFn env evAp $ List.fromFoldable [ pure $ Expr fn, pure $ Expr $ ApplyTree args ]
        doCase _ = sym

int' :: Partial => Value
int' = Define $ Env $ Map.fromFoldable
    [ Tuple "add" $ function 2 add'
    , Tuple "sub" $ function 2 sub'
    , Tuple "mul" $ function 2 mul'
    , Tuple "div" $ function 3 div'
    , Tuple "lt" $ function 4 lt'
    , Tuple "gt" $ function 4 gt'
    ]
  where
    add' env (a : b : Nil) = do
      evA <- asInteger a
      evB <- asInteger b
      pure $ IntValue $ evA + evB

    sub' env (a : b : Nil) = do
      evA <- asInteger a
      evB <- asInteger b
      pure $ IntValue $ evA - evB

    mul' env (a : b : Nil) = do
      evA <- asInteger a
      evB <- asInteger b
      pure $ IntValue $ evA * evB

    div' env (a : b : zero : Nil) = do
      evA <- asInteger a
      evB <- asInteger b
      if evB == fromInt 0 then zero else pure $ IntValue $ evA / evB

    lt' env (a : b : t' : f' : Nil) = do
      evA <- asInteger a
      evB <- asInteger b
      if evA < evB then t' else f'

    gt' env (a : b : t' : f' : Nil) = do
      evA <- asInteger a
      evB <- asInteger b
      if evA > evB then t' else f'

char' :: Partial => Value
char' = Define $ Env $ Map.fromFoldable [ Tuple "eq" $ function 4 eqChar' ]
  where
    eqChar' env (a : b : t' : f' : Nil) = do
      evA <- asChar a
      evB <- asChar b
      if evA == evB then t' else f'

string' :: Partial => Value
string' = Define $ Env $ Map.fromFoldable [ Tuple "case" $ function 3 case' ]
  where
    case' env (expr : nil : cons : Nil) = asString expr <#> (String.toCharArray >>> List.fromFoldable) >>= doCase
      where
        doCase Nil = nil
        doCase (a : b) = do
          evCons <- cons
          applyFn env evCons $ List.fromFoldable
            [ pure (CharValue a)
            , pure (StringValue $ String.fromCharArray $ Array.fromFoldable b)
            ]
