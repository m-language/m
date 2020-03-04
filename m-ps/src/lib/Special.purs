module Special where

import Control.Monad.Except (throwError)
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.List ((:), List(..))
import Data.List as List
import Data.Map as Map
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..), curry)
import Eval (Env(..), Error(..), EvalResult, Process(..), Value(..), applyFn, asChar, asExpr, asInteger, asProcess, asString, eval, evalBlock, insertEnv, insertEnvLazy)
import Prelude (class Monad, bind, pure, show, ($), (*), (+), (-), (/), (<), (<#>), (<$>), (<<<), (<>), (==), (>), (>>=), (>>>))
import Tree (Tree(..))

value :: forall m. Monad m => String -> Value -> Tuple String (m Value)
value name v = Tuple name $ pure v

function :: forall m. Monad m => String -> Int -> (Env -> List (EvalResult Value) -> EvalResult Value) -> Tuple String (m Value)
function name i f = Tuple name $ pure $ Function i f

macro :: forall m. Monad m => String -> Int -> (Env -> List Tree -> EvalResult Value) -> Tuple String (m Value)
macro name i f = Tuple name $ pure $ Macro i f

special :: Partial => Env
special = Env $ Map.fromFoldable
    [ macro "fn" 2 fn'
    , macro "fm" 2 fm'
    , macro "def" 2 def'
    , macro "block" 1 block'
    , macro "quote" 1 quote'
    , function "error" 1 error'
    , value "expr-ops" expr'
    , value "int-ops" int'
    , value "char-ops" char'
    , value "string-ops" string'
    , value "process-ops" process'
    ]

getNames :: Partial => Tree -> EvalResult (List String)
getNames (SymbolTree name) = pure $ name : Nil
getNames (ApplyTree args) = names args
  where
    names :: List Tree -> EvalResult (List String)
    names Nil = pure Nil
    names ((SymbolTree name) : cdr) = names cdr <#> ((:) name)
    names (ap@(ApplyTree _) : cdr) = throwError $ Error $ "Expected symbol, found " <> show ap

fn' :: Partial => Env -> List Tree -> EvalResult Value
fn' closure (argsNames : expr : Nil) = do
  names <- getNames argsNames
  pure $ Function (List.length names) \env args ->
      fnApply closure names args expr

fnApply :: Partial => Env -> List String -> List (EvalResult Value) -> Tree -> EvalResult Value
fnApply closure Nil Nil tree = eval (Tuple closure tree)
fnApply closure (name : names) (arg : args) tree = 
  fnApply (insertEnvLazy name arg closure) names args tree

fm' :: Partial => Env -> List Tree -> EvalResult Value
fm' closure (argNames : expr : Nil) = do
  names <- getNames argNames
  pure $ Macro (List.length names) \env args ->
      fmApply env closure names args expr

fmApply :: Partial => Env -> Env -> List String -> List Tree -> Tree -> EvalResult Value
fmApply env closure Nil Nil tree = asExpr (eval (Tuple closure tree)) >>= curry eval env
fmApply env closure (name : names) (arg : args) tree = 
  fmApply env (insertEnv name (Expr arg) closure) names args tree

def' :: Partial => Env -> List Tree -> EvalResult Value
def' env (names : expr : Nil) = case names of
  SymbolTree name -> pure $ Define $ Env $ Map.singleton name $ eval $ Tuple env expr
  x -> throwError $ Error $ "Expected symbol, found " <> show x

block' :: Partial => Env -> List Tree -> EvalResult Value
block' env (exprs : Nil) = case exprs of
  SymbolTree name -> eval (Tuple env (SymbolTree name))
  ApplyTree args -> evalBlock env args <#> Define

quote' :: Partial => Env -> List Tree -> EvalResult Value
quote' env (tree : Nil) = pure $ Expr tree

error' :: Partial => Env -> List (EvalResult Value) -> EvalResult Value
error' env (expr : Nil) = asString expr >>= (throwError <<< Error)

expr' :: Partial => Value
expr' = Define $ Env $ Map.fromFoldable [ function "case" 4 case' ]
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
    [ function "add" 2 add'
    , function "sub" 2 sub'
    , function "mul" 2 mul'
    , function "div" 3 div'
    , function "lt" 4 lt'
    , function "gt" 4 gt'
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
char' = Define $ Env $ Map.fromFoldable [ function "eq" 4 eqChar' ]
  where
    eqChar' env (a : b : t' : f' : Nil) = do
      evA <- asChar a
      evB <- asChar b
      if evA == evB then t' else f'

string' :: Partial => Value
string' = Define $ Env $ Map.fromFoldable [ function "case" 3 case' ]
  where
    case' env (expr : nil : cons : Nil) = asString expr <#> (String.toCharArray >>> List.fromFoldable) >>= doCase
      where
        doCase Nil = nil
        doCase (a : b) = do
          evCons <- cons
          applyFn env evCons
            ( List.fromFoldable
                [ pure (CharValue a)
                , pure (StringValue $ String.fromCharArray $ Array.fromFoldable b)
                ]
            )

process' :: Partial => Value
process' = Define $ Env $ Map.fromFoldable [ function "do" 2 do', function "impure" 1 impure' ]
  where
    do' env (proc : map : Nil) = do
      process <- asProcess proc
      evMap <- map
      pure $ ProcessValue $ Do
          process
          \arg -> asProcess $ applyFn env evMap $ List.singleton $ pure arg

    impure' env (val : Nil) = ProcessValue <<< Impure <<< pure <$> val
