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
    | CharValue Char
    | StringValue String
    | IntegerValue Integer
    | Expr Tree

data Error
    = Error String
    | Undefined (Set String)

instance Show Value where
    show (Function n f       ) = "<function[" ++ show n ++ "]>"
    show (CharValue    char  ) = show char
    show (StringValue  string) = show string
    show (IntegerValue i     ) = show i
    show (Expr         tree  ) = "'" ++ show tree

instance Show Error where
    show (Error     string) = "Error: " ++ string
    show (Undefined ns    ) = "Undefined: " ++ show (map Symbol $ Set.toList ns)

newtype Env = Env (Map String (EvalResult Value))

insertEnv :: String -> Value -> Env -> Env
insertEnv name value (Env env) = Env $ Map.insert name (return value) env

insertEnvLazy :: String -> EvalResult Value -> Env -> Env
insertEnvLazy name value (Env env) = Env $ Map.insert name value env

lookupEnv :: String -> Env -> Maybe (EvalResult Value)
lookupEnv name (Env env) = Map.lookup name env

type EvalResult = StateT Env (Either Error)

evalSeq :: Env -> [Tree] -> EvalResult ()
evalSeq env = evalSeq' env False Set.empty []
  where
    evalSeq' :: Env -> Bool -> Set String -> [Tree] -> [Tree] -> EvalResult ()
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
eval (env, (CharTree char)      ) = return $ CharValue char
eval (env, (StringTree string)  ) = return $ StringValue string
eval (env, (IntegerTree integer)) = return $ IntegerValue integer
eval (env, (Apply fn args)      ) = do
    f <- eval (env, fn)
    apply env f $ map (env, ) args

evalToExpr :: (Env, Tree) -> EvalResult Tree
evalToExpr tree = eval tree >>= \case
    (Expr tree) -> return tree
    x           -> throwError $ Error $ "Expected expression, found " ++ show x

evalToSymbol :: (Env, Tree) -> EvalResult String
evalToSymbol tree = evalToExpr tree >>= \case
    (Symbol s) -> return s
    x          -> throwError $ Error $ "Expected symbol, found " ++ show x

evalToChar :: (Env, Tree) -> EvalResult Char
evalToChar tree = eval tree >>= \case
    (CharValue char) -> return char
    x -> throwError $ Error $ "Expected character, found " ++ show x

evalToString :: (Env, Tree) -> EvalResult String
evalToString tree = eval tree >>= \case
    (StringValue string) -> return string
    x -> throwError $ Error $ "Expected string, found " ++ show x

evalToInteger :: (Env, Tree) -> EvalResult Integer
evalToInteger tree = eval tree >>= \case
    (IntegerValue i) -> return i
    x -> throwError $ Error $ "Expected integer, found " ++ show x

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
apply env x args = throwError $ Error $ "Expected function, found " ++ show x
