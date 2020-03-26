module Special where

import Prelude

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, local)
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.List ((:), List(..), length)
import Data.List as List
import Data.Map (insert, union)
import Data.Map as Map
import Data.String.CodeUnits as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), curry)
import Eval (applyFn, eval, evalBlock, function, macro)
import Eval.Types (Env, Error(..), EvalResult, Process(..), Value(..), asChar, asExpr, asInteger, asString)
import Tree (Tree(..))

special :: Partial => Env
special = Map.fromFoldable
    [ Tuple "fn" $ pure $ macro 2 fn'
    , Tuple "fm" $ pure $ macro 2 fm'
    , Tuple "def"  $ pure $ macro 2 def'
    , Tuple "block" $ pure $ macro 1 block'
    , Tuple "quote" $ pure $ macro 1 quote'
    , Tuple "error" $ pure $ function 1 error'
    , Tuple "impure" $ pure $ function 1 impure' 
    , Tuple "expr-ops" $ pure $ expr'
    , Tuple "int-ops" $ pure $ int'
    , Tuple "char-ops" $ pure $ char'
    , Tuple "string-ops" $ pure $ string'
    ]

getNames :: Tree -> EvalResult (List String)
getNames (SymbolTree name) = pure $ List.singleton name
getNames (ListTree trees) = List.concat <$> traverse getNames trees
getNames tree = throwError $ Error $ "Expected list, found " <> show tree

fn' :: Partial => Env -> List Tree -> EvalResult Value
fn' closure (argsNames : expr : Nil) = do
  names <- getNames argsNames
  globalClosure <- ask
  case names of
    Nil -> eval $ Tuple closure expr
    list -> pure $ function (length names) \env args ->
        local (\global -> union global globalClosure) $ fnApply closure names args expr

fnApply :: Partial => Env -> List String -> List (EvalResult Value) -> Tree -> EvalResult Value
fnApply closure Nil Nil tree = eval $ Tuple closure tree
fnApply closure (name : names) (arg : args) tree = 
  fnApply (insert name arg closure) names args tree

fm' :: Partial => Env -> List Tree -> EvalResult Value
fm' closure (argNames : expr : Nil) = do
  names <- getNames argNames
  globalClosure <- ask
  case names of
    Nil -> eval $ Tuple closure expr
    list -> pure $ macro (length names) \env args ->
        (local (\global -> union global globalClosure) $ fmApply env closure names args expr) >>= curry eval env

fmApply :: Partial => Env -> Env -> List String -> List Tree -> Tree -> EvalResult Tree
fmApply env closure Nil Nil tree = asExpr $ eval $ Tuple closure tree
fmApply env closure (name : names) (arg : args) tree = 
  fmApply env (insert name (pure $ Expr arg) closure) names args tree

def' :: Partial => Env -> List Tree -> EvalResult Value
def' env (names : expr : Nil) = case names of
  SymbolTree name -> do
    closure <- ask
    pure $ Define $ Map.singleton name $ local (\global -> union global closure) $ eval $ Tuple env expr
  x -> throwError $ Error $ "Expected symbol, found " <> show x

block' :: Partial => Env -> List Tree -> EvalResult Value
block' env ((ListTree trees) : Nil) = Define <$> evalBlock env trees
block' env (tree : Nil) = throwError $ Error $ "Expected list, found " <> show tree

quote' :: Partial => Env -> List Tree -> EvalResult Value
quote' env (tree : Nil) = pure $ Expr tree  

error' :: Partial => Env -> List (EvalResult Value) -> EvalResult Value
error' env (expr : Nil) = asString expr >>= (throwError <<< Error)

impure' :: Partial => Env -> List (EvalResult Value) -> EvalResult Value
impure' env (val : Nil) = ProcessValue <<< Impure <<< pure <$> val

expr' :: Partial => Value
expr' = Define $ Map.fromFoldable [ Tuple "case" $ pure $ function 4 case' ]
  where
    case' env (expr : sym : ap : list : Nil) = asExpr expr >>= doCase
      where
        doCase (ApplyTree fn arg) = do
          evAp <- ap
          applyFn env evAp $ List.fromFoldable [ pure $ Expr fn, pure $ Expr arg ]
        doCase (ListTree trees) = do
          evList <- list
          applyFn env evList $ List.fromFoldable [ pure $ function 2 $ listCase' trees ]
            where
              listCase' :: Partial => List Tree -> Env -> List (EvalResult Value) -> EvalResult Value
              listCase' Nil env' (nil : cons : Nil) = nil
              listCase' (car : cdr) env' (nil : cons : Nil) = do
                evCons <- cons
                applyFn env' evCons $ List.fromFoldable [ pure $ Expr car, pure $ Expr $ ListTree cdr ]
        doCase _ = sym

int' :: Partial => Value
int' = Define $ Map.fromFoldable
    [ Tuple "add" $ pure $ function 2 add'
    , Tuple "sub" $ pure $ function 2 sub'
    , Tuple "mul" $ pure $ function 2 mul'
    , Tuple "div" $ pure $ function 3 div'
    , Tuple "lt" $ pure $ function 4 lt'
    , Tuple "gt" $ pure $ function 4 gt'
    , Tuple "eq" $ pure $ function 4 eq'
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
    
    eq' env (a : b : t' : f' : Nil) = do
      evA <- asInteger a
      evB <- asInteger b
      if evA == evB then t' else f'

char' :: Partial => Value
char' = Define $ Map.fromFoldable [ Tuple "eq" $ pure $ function 4 eqChar' ]
  where
    eqChar' env (a : b : t' : f' : Nil) = do
      evA <- asChar a
      evB <- asChar b
      if evA == evB then t' else f'

string' :: Partial => Value
string' = Define $ Map.fromFoldable [ Tuple "case" $ pure $ function 3 case' ]
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
