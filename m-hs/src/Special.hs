{-# LANGUAGE LambdaCase #-}

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

entry name i f = (name, return $ Function i f)

special :: Env
special = Env $ Map.fromList
    [ ("expr-ops"   , return expr')
    , ("int-ops"    , return int')
    , ("char-ops"   , return char')
    , ("string-ops" , return string')
    , ("process-ops", return process')
    , entry "fn"    2 fn'
    , entry "fm"    2 fm'
    , entry "def"   2 def'
    , entry "block" 1 block'
    , entry "error" 1 error'
    , entry "quote" 1 quote'
    ]

getNames :: Tree -> EvalResult [String]
getNames (Symbol name) = return [name]
getNames (Apply  args) = names args
  where
    names :: [Tree] -> EvalResult [String]
    names []                    = return []
    names ((Symbol name) : cdr) = names cdr <&> (name :)
    names (ap@(Apply _) : cdr) =
        throwError $ Error $ "Expected symbol, found " ++ show ap

fn' :: Env -> [(Env, Tree)] -> EvalResult Value
fn' closure [args, value] = do
    names <- getNames $ snd args
    return $ Function (length names) $ \env args ->
        fnApply closure names args $ snd value

fnApply :: Env -> [String] -> [(Env, Tree)] -> Tree -> EvalResult Value
fnApply closure [] [] tree = eval (closure, tree)
fnApply closure (name : names) (arg : args) tree =
    fnApply (insertEnvLazy name (eval arg) closure) names args tree

fm' :: Env -> [(Env, Tree)] -> EvalResult Value
fm' closure [args, value] = do
    names <- getNames $ snd args
    return $ Function (length names) $ \env args ->
        fmApply env closure names args $ snd value

fmApply :: Env -> Env -> [String] -> [(Env, Tree)] -> Tree -> EvalResult Value
fmApply env closure [] [] tree = evalToExpr (closure, tree) >>= curry eval env
fmApply env closure (name : names) (arg : args) tree =
    fmApply env (insertEnv name (Expr $ snd arg) closure) names args tree

def' :: Env -> [(Env, Tree)] -> EvalResult Value
def' env [names, value] = case snd names of
    Symbol name -> return $ Define $ Env $ Map.singleton name $ eval value
    x           -> throwError $ Error $ "Expected symbol, found " ++ show x

block' :: Env -> [(Env, Tree)] -> EvalResult Value
block' env [exprs] = case snd exprs of
    Symbol name -> eval (env, Symbol name)
    Apply  args -> evalBlock env args <&> Define

error' :: Env -> [(Env, Tree)] -> EvalResult Value
error' env [expr] = evalToString expr >>= \e -> throwError $ Error e

quote' :: Env -> [(Env, Tree)] -> EvalResult Value
quote' env [tree] = return $ Expr $ snd tree

expr' :: Value
expr' = Define $ Env $ Map.fromList [entry "case" 6 case']
  where
    case' env [expr, symArgs, sym, nil, apArgs, ap] =
        evalToExpr expr >>= doCase
      where
        doCase (Apply []         ) = eval (env, snd nil)
        doCase (Apply (fn : args)) = getNames (snd apArgs) >>= \case
            [fnName, argName] ->
                let env'  = insertEnv fnName (Expr fn) (fst ap)
                    env'' = insertEnv argName (Expr $ Apply args) env'
                in  eval (env'', snd ap)
            xs -> throwError $ Error "Apply case should have 2 arguments"
        doCase expr = getNames (snd symArgs) >>= \case
            [symName] ->
                let env' = insertEnv symName (Expr expr) (fst sym)
                in  eval (env', snd sym)

int' :: Value
int' = Define $ Env $ Map.fromList
    [ entry "add" 2 add'
    , entry "sub" 2 sub'
    , entry "mul" 2 mul'
    , entry "div" 3 div'
    , entry "lt"  4 lt'
    , entry "gt"  4 gt'
    ]
  where
    add' env [a, b] = do
        evA <- evalToInteger a
        evB <- evalToInteger b
        return $ IntValue $ evA + evB
    sub' env [a, b] = do
        evA <- evalToInteger a
        evB <- evalToInteger b
        return $ IntValue $ evA - evB
    mul' env [a, b] = do
        evA <- evalToInteger a
        evB <- evalToInteger b
        return $ IntValue $ evA * evB
    div' env [a, b, zero] = do
        evA <- evalToInteger a
        evB <- evalToInteger b
        if evB == 0 then eval zero else return $ IntValue $ evA `quot` evB
    lt' env [int, int', t', f'] = do
        int  <- evalToInteger int
        int' <- evalToInteger int'
        if int < int' then eval t' else eval f'
    gt' env [int, int', t', f'] = do
        int  <- evalToInteger int
        int' <- evalToInteger int'
        if int > int' then eval t' else eval f'

char' :: Value
char' = Define $ Env $ Map.fromList [entry "eq" 4 eqChar']
  where
    eqChar' env [char, char', t', f'] = do
        evChar  <- evalToChar char
        evChar' <- evalToChar char'
        if evChar == evChar' then eval t' else eval f'

string' :: Value
string' = Define $ Env $ Map.fromList [entry "case" 4 case']
  where
    case' env [expr, nil, consArgs, cons] = evalToString expr >>= doCase
      where
        doCase []          = eval nil
        doCase (car : cdr) = getNames (snd consArgs) >>= \case
            [fnName, argName] ->
                let env'  = insertEnv fnName (CharValue car) (fst cons)
                    env'' = insertEnv argName (StringValue cdr) env'
                in  eval (env'', snd cons)
            xs -> throwError $ Error "Cons case should have 2 arguments"

process' :: Value
process' = Define $ Env $ Map.fromList
    [entry "do" 3 do', entry "impure" 1 impure']
  where
    do' env [proc, names, map] = case snd names of
        Symbol name -> do
            process <- evalToProcess proc
            return $ ProcessValue $ Do
                process
                (\arg -> evalToProcess (insertEnv name arg env, snd map))
        x -> throwError $ Error $ "Expected symbol, found " ++ show x
    impure' env [value] = do
        evValue <- eval value
        return $ ProcessValue $ Impure $ return evValue
