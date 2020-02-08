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

special :: Env
special = Env $ Map.fromList
    [ entry "fn"          2 fn'
    , entry "fm"          2 fm'
    , entry "def"         2 def'
    , entry "block"       1 block'
    , entry "error"       1 error'
    , entry "quote"       1 quote'
    , entry "case@expr"   6 caseExpr'
    , entry "eq@char"     4 eqChar'
    , entry "case@string" 4 caseString'
    , entry "add@int"     2 addInt'
    , entry "sub@int"     2 subInt'
    , entry "mul@int"     2 mulInt'
    , entry "div@int"     3 divInt'
    , entry "lt@int"      4 ltInt'
    , entry "gt@int"      4 gtInt'
    , entry "do@process"  3 doProcess'
    ]
    where entry name i f = (name, return $ Function i f)

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
    Symbol name -> return $ Define [(name, eval value)]
    x           -> throwError $ Error $ "Expected symbol, found " ++ show x

block' :: Env -> [(Env, Tree)] -> EvalResult Value
block' env [exprs] = case snd exprs of
    Symbol name -> eval (env, Symbol name)
    Apply  args -> evalBlock env args <&> Define

error' :: Env -> [(Env, Tree)] -> EvalResult Value
error' env [expr] = evalToString expr >>= \e -> throwError $ Error e

caseExpr' :: Env -> [(Env, Tree)] -> EvalResult Value
caseExpr' env [expr, symArgs, sym, nil, apArgs, ap] =
    evalToExpr expr >>= doCase
  where
    doCase (Apply []) = eval (env, snd nil)
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

eqChar' :: Env -> [(Env, Tree)] -> EvalResult Value
eqChar' env [char, char', t', f'] = do
    evChar  <- evalToChar char
    evChar' <- evalToChar char'
    if evChar == evChar' then eval t' else eval f'

quote' :: Env -> [(Env, Tree)] -> EvalResult Value
quote' env [tree] = return $ Expr $ snd tree

caseString' :: Env -> [(Env, Tree)] -> EvalResult Value
caseString' env [expr, nil, consArgs, cons] = evalToString expr >>= doCase
  where
    doCase []          = eval nil
    doCase (car : cdr) = getNames (snd consArgs) >>= \case
        [fnName, argName] ->
            let env'  = insertEnv fnName (CharValue car) (fst cons)
                env'' = insertEnv argName (StringValue cdr) env'
            in  eval (env'', snd cons)
        xs -> throwError $ Error "Cons case should have 2 arguments"

addInt' :: Env -> [(Env, Tree)] -> EvalResult Value
addInt' env [a, b] = do
    evA <- evalToInteger a
    evB <- evalToInteger b
    return $ IntValue $ evA + evB

subInt' :: Env -> [(Env, Tree)] -> EvalResult Value
subInt' env [a, b] = do
    evA <- evalToInteger a
    evB <- evalToInteger b
    return $ IntValue $ evA - evB

mulInt' :: Env -> [(Env, Tree)] -> EvalResult Value
mulInt' env [a, b] = do
    evA <- evalToInteger a
    evB <- evalToInteger b
    return $ IntValue $ evA * evB

divInt' :: Env -> [(Env, Tree)] -> EvalResult Value
divInt' env [a, b, zero] = do
    evA <- evalToInteger a
    evB <- evalToInteger b
    if evB == 0 then eval zero else return $ IntValue $ evA `quot` evB

ltInt' :: Env -> [(Env, Tree)] -> EvalResult Value
ltInt' env [int, int', t', f'] = do
    int  <- evalToInteger int
    int' <- evalToInteger int'
    if int < int' then eval t' else eval f'

gtInt' :: Env -> [(Env, Tree)] -> EvalResult Value
gtInt' env [int, int', t', f'] = do
    int  <- evalToInteger int
    int' <- evalToInteger int'
    if int > int' then eval t' else eval f'

doProcess' :: Env -> [(Env, Tree)] -> EvalResult Value
doProcess' env [proc, names, map] = case snd names of
    Symbol name -> do
        process <- evalToProcess proc
        return $ ProcessValue $ Do
            process
            (\arg -> evalToProcess (insertEnv name arg env, snd map))
    x -> throwError $ Error $ "Expected symbol, found " ++ show x
