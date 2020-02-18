module Special where

import           Eval
import           Tree
import           Data.HashMap                   ( Map )
import qualified Data.HashMap                  as Map
import           Data.HashSet                   ( Set )
import qualified Data.HashSet                  as Set
import           Data.Functor
import           Data.Bifunctor
import           Data.Functor.Classes
import           Control.Monad.State
import           Control.Monad.Except

value name v = (name, return v)

function name i f = (name, return $ Function i f)

macro name i f = (name, return $ Macro i f)

special :: Env
special = Env $ Map.fromList
    [ macro "fn"    2 fn'
    , macro "fm"    2 fm'
    , macro "def"   2 def'
    , macro "block" 1 block'
    , macro "quote" 1 quote'
    , function "error" 1 error'
    , value "expr-ops"    expr'
    , value "int-ops"     int'
    , value "char-ops"    char'
    , value "string-ops"  string'
    , value "process-ops" process'
    ]

getNames :: Tree -> EvalResult [String]
getNames (SymbolTree name) = return [name]
getNames (ApplyTree args) = names args
  where
    names :: [Tree] -> EvalResult [String]
    names []                    = return []
    names ((SymbolTree name) : cdr) = names cdr <&> (name :)
    names (ap@(ApplyTree _) : cdr) =
        throwError $ Error $ "Expected symbol, found " ++ show ap

fn' :: Env -> [Tree] -> EvalResult Value
fn' closure [args, value] = do
    names <- getNames args
    return $ Function (length names) $ \env args ->
        fnApply closure names args value

fnApply :: Env -> [String] -> [EvalResult Value] -> Tree -> EvalResult Value
fnApply closure [] [] tree = eval (closure, tree)
fnApply closure (name : names) (arg : args) tree =
    fnApply (insertEnvLazy name arg closure) names args tree

fm' :: Env -> [Tree] -> EvalResult Value
fm' closure [args, value] = do
    names <- getNames args
    return $ Macro (length names) $ \env args ->
        fmApply env closure names args value

fmApply :: Env -> Env -> [String] -> [Tree] -> Tree -> EvalResult Value
fmApply env closure [] [] tree =
    asExpr (eval (closure, tree)) >>= curry eval env
fmApply env closure (name : names) (arg : args) tree =
    fmApply env (insertEnv name (Expr arg) closure) names args tree

def' :: Env -> [Tree] -> EvalResult Value
def' env [names, value] = case names of
    SymbolTree name ->
        return $ Define $ Env $ Map.singleton name $ eval (env, value)
    x -> throwError $ Error $ "Expected symbol, found " ++ show x

block' :: Env -> [Tree] -> EvalResult Value
block' env [exprs] = case exprs of
    SymbolTree name -> eval (env, SymbolTree name)
    ApplyTree  args -> evalBlock env args <&> Define

quote' :: Env -> [Tree] -> EvalResult Value
quote' env [tree] = return $ Expr tree

error' :: Env -> [EvalResult Value] -> EvalResult Value
error' env [expr] = asString expr >>= \e -> throwError $ Error e

expr' :: Value
expr' = Define $ Env $ Map.fromList [function "case" 4 case']
  where
    case' env [expr, sym, nil, ap] = asExpr expr >>= doCase
      where
        doCase (ApplyTree []         ) = nil
        doCase (ApplyTree (fn : args)) = do
            evAp <- ap
            applyFn env evAp [return (Expr fn), return (Expr $ ApplyTree args)]
        doCase expr = sym

int' :: Value
int' = Define $ Env $ Map.fromList
    [ function "add" 2 add'
    , function "sub" 2 sub'
    , function "mul" 2 mul'
    , function "div" 3 div'
    , function "lt"  4 lt'
    , function "gt"  4 gt'
    ]
  where
    add' env [a, b] = do
        evA <- asInteger a
        evB <- asInteger b
        return $ IntValue $ evA + evB
    sub' env [a, b] = do
        evA <- asInteger a
        evB <- asInteger b
        return $ IntValue $ evA - evB
    mul' env [a, b] = do
        evA <- asInteger a
        evB <- asInteger b
        return $ IntValue $ evA * evB
    div' env [a, b, zero] = do
        evA <- asInteger a
        evB <- asInteger b
        if evB == 0 then zero else return $ IntValue $ evA `quot` evB
    lt' env [int, int', t', f'] = do
        int  <- asInteger int
        int' <- asInteger int'
        if int < int' then t' else f'
    gt' env [int, int', t', f'] = do
        int  <- asInteger int
        int' <- asInteger int'
        if int > int' then t' else f'

char' :: Value
char' = Define $ Env $ Map.fromList [function "eq" 4 eqChar']
  where
    eqChar' env [char, char', t', f'] = do
        evChar  <- asChar char
        evChar' <- asChar char'
        if evChar == evChar' then t' else f'

string' :: Value
string' = Define $ Env $ Map.fromList [function "case" 3 case']
  where
    case' env [expr, nil, cons] = asString expr >>= doCase
      where
        doCase []          = nil
        doCase (a : b) = do
            evCons <- cons
            applyFn env evCons [return (CharValue a), return (StringValue b)]

process' :: Value
process' = Define $ Env $ Map.fromList
    [function "do" 2 do', function "impure" 1 impure']
  where
    do' env [proc, map] = do
        process <- asProcess proc
        evMap <- map
        return $ ProcessValue $ Do
            process
            (\arg -> asProcess $ applyFn env evMap [return arg])
    impure' env [value] = ProcessValue . Impure . return <$> value
