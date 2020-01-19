{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

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

data Value
    = Function Int (Env -> [(Env, Tree)] -> EvalResult Value)
    | Expr Tree

data Error
    = Error String
    | Undefined (Set [String])

instance Show Value where
    show (Function n f) = "<function[" ++ show n ++ "]>"
    show (Expr tree   ) = "'" ++ show tree

instance Show Error where
    show (Error     string) = "Error: " ++ string
    show (Undefined ns    ) = "Undefined: " ++ show (map Symbol $ Set.toList ns)

newtype Env = Env (Map [String] (EvalResult Value))

insertEnv :: [String] -> Value -> Env -> Env
insertEnv name value (Env env) = Env $ Map.insert name (return value) env

insertEnvLazy :: [String] -> EvalResult Value -> Env -> Env
insertEnvLazy name value (Env env) = Env $ Map.insert name value env

lookupEnv :: [String] -> Env -> Maybe (EvalResult Value)
lookupEnv name (Env env) = Map.lookup name env

type EvalResult = StateT Env (Either Error)

evalSeq :: Env -> [Tree] -> EvalResult ()
evalSeq env = evalSeq' env False Set.empty []
  where
    evalSeq' :: Env -> Bool -> Set [String] -> [Tree] -> [Tree] -> EvalResult ()
    evalSeq' env found errors []    [] = return ()
    evalSeq' env found errors defer [] = if found
        then evalSeq' env False Set.empty [] defer
        else throwError $ Undefined errors
    evalSeq' env found errors defer (car : cdr) = do
        globals <- get
        case runStateT (eval (env, car)) globals of
            Right value ->
                eval (env, car) >> evalSeq' env True errors defer cdr
            Left (Undefined names) ->
                evalSeq' env found (Set.union names errors) (car : defer) cdr
            Left (Error string) -> throwError $ Error string

eval :: (Env, Tree) -> EvalResult Value
eval (env, (Symbol name)) = get >>= \globals -> case lookupEnv name env of
    Just value -> value
    Nothing    -> case lookupEnv name globals of
        Just value -> value
        Nothing    -> throwError $ Undefined $ Set.singleton name
eval (env, (Apply fn args)) = do
    f <- eval (env, fn)
    apply env f $ map (env, ) args

evalToExpr :: (Env, Tree) -> EvalResult Tree
evalToExpr tree = eval tree >>= \case
    (Expr tree) -> return tree
    x           -> throwError $ Error $ "Expected expression, found " ++ show x

evalToSymbol :: (Env, Tree) -> EvalResult [String]
evalToSymbol tree = evalToExpr tree >>= \case
    (Symbol s) -> return s
    x          -> throwError $ Error $ "Expected symbol, found " ++ show x

apply :: Env -> Value -> [(Env, Tree)] -> EvalResult Value
apply env (Function n f) args =
    let argsLength = length args
    in  if length args < n
            then return $ Function (n - argsLength) $ \env args' ->
                apply env (Function n f) (args ++ args')
            else if argsLength > n
                then apply env (Function n f) (take n args)
                    >>= \f -> apply env f (drop n args)
                else f env args
apply env (Expr tree) args = do
    evArgs <- mapM evalToExpr args
    return $ Expr $ if null evArgs then tree else Apply tree evArgs

special :: Env
special = Env $ Map.fromList
    [ (["fn"]              , return $ Function 2 fn')
    , (["fm"]              , return $ Function 2 fm')
    , (["def"]             , return $ Function 2 def')
    , (["block"]           , return $ Function 2 block')
    , (["error"]           , return $ Function 1 error')
    , (["expr", "case"]    , return $ Function 7 case')
    , (["symbol", "eq"]    , return $ Function 4 eq')
    , (["symbol", "concat"], return $ Function 2 concat')
    , (["quote"]           , return $ Function 1 quote')
    ]

getNames :: Tree -> EvalResult [[String]]
getNames (Symbol name  ) = return [name]
getNames (Apply fn args) = do
    fnNames  <- names [fn]
    argNames <- names args
    return $ fnNames ++ argNames
  where
    names :: [Tree] -> EvalResult [[String]]
    names []                    = return []
    names ((Symbol name) : cdr) = names cdr <&> (name :)
    names (ap@(Apply _ _) : cdr) =
        throwError $ Error $ "Expected symbol, found " ++ show ap

fn' :: Env -> [(Env, Tree)] -> EvalResult Value
fn' closure [args, value] = do
    names <- getNames $ snd args
    return $ Function (length names) $ \env args ->
        fnApply closure names args $ snd value

fnApply :: Env -> [[String]] -> [(Env, Tree)] -> Tree -> EvalResult Value
fnApply closure [] [] tree = eval (closure, tree)
fnApply closure (name : names) (arg : args) tree =
    fnApply (insertEnvLazy name (eval arg) closure) names args tree

fm' :: Env -> [(Env, Tree)] -> EvalResult Value
fm' closure [args, value] = do
    names <- getNames $ snd args
    return $ Function (length names) $ \env args ->
        fmApply env closure names args $ snd value

fmApply :: Env -> Env -> [[String]] -> [(Env, Tree)] -> Tree -> EvalResult Value
fmApply env closure [] [] tree = evalToExpr (closure, tree) >>= curry eval env
fmApply env closure (name : names) (arg : args) tree =
    fmApply env (insertEnv name (Expr $ snd arg) closure) names args tree

def' :: Env -> [(Env, Tree)] -> EvalResult Value
def' env [names, value] = case snd names of
    Symbol name -> do
        env <- get
        ev  <- eval value
        case lookupEnv name env of
            Nothing -> modify $ insertEnv name ev
            Just a ->
                throwError $ Error $ "Cannot redefine " ++ show (Symbol name)
        return ev
    x -> throwError $ Error $ "Expected symbol, found " ++ show x

block' :: Env -> [(Env, Tree)] -> EvalResult Value
block' env [result, exprs] = case snd exprs of
    Symbol name   -> eval (env, Symbol name)
    Apply fn args -> do
        evalSeq env (fn : args)
        eval result

error' :: Env -> [(Env, Tree)] -> EvalResult Value
error' env [expr] = evalToExpr expr >>= \e -> throwError $ Error $ show e

case' :: Env -> [(Env, Tree)] -> EvalResult Value
case' env [expr, symArgs, sym, nilArgs, nil, apArgs, ap] =
    evalToExpr expr >>= doCase
  where
    doCase (Symbol name) = getNames (snd symArgs) >>= \case
        [symName] ->
            let env' = insertEnv symName (Expr $ Symbol name) (fst sym)
            in  eval (env', snd sym)
    doCase (Apply fn []) = getNames (snd nilArgs) >>= \case
        [fnName] ->
            let env' = insertEnv fnName (Expr fn) (fst nil)
            in  eval (env', snd nil)
    doCase (Apply fn (car : cdr)) = getNames (snd apArgs) >>= \case
        [fnName, argName] ->
            let env'  = insertEnv fnName (Expr fn) (fst ap)
                env'' = insertEnv argName (Expr $ Apply car cdr) env'
            in  eval (env'', snd ap)
        xs -> throwError $ Error "Apply case should have 2 arguments"

eq' :: Env -> [(Env, Tree)] -> EvalResult Value
eq' env [expr, expr', t', f'] = do
    sym  <- evalToSymbol expr
    sym' <- evalToSymbol expr'
    if sym == sym' then eval t' else eval f'

concat' :: Env -> [(Env, Tree)] -> EvalResult Value
concat' env [expr, expr'] = do
    sym  <- evalToSymbol expr
    sym' <- evalToSymbol expr'
    return $ Expr $ Symbol (sym ++ sym')

quote' :: Env -> [(Env, Tree)] -> EvalResult Value
quote' env [tree] = return $ Expr $ snd tree
