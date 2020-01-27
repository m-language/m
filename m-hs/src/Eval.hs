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
import           Data.List
import           Control.Monad.Except
import           Control.Monad.Reader

type Defs = [(String, EvalResult Value)]

data Value
    = Function Int (Env -> [(Env, Tree)] -> EvalResult Value)
    | Define Defs
    | Expr Tree
    | CharValue Char
    | StringValue String
    | IntegerValue Integer

data Error
    = Error String
    | Undefined (Set String)

instance Show Value where
    show (Function n f  ) = "<function>"
    show (Expr         t) = "'" ++ show t
    show (CharValue    c) = show c
    show (StringValue  s) = show s
    show (IntegerValue i) = show i
    show (Define       d) = unwords $ map (\(n, v) -> n) d

instance Show Error where
    show (Error     string) = "Error: " ++ string
    show (Undefined ns    ) = "Undefined: " ++ show (map Symbol $ Set.toList ns)

newtype Env = Env (Map String (EvalResult Value))

insertEnv :: String -> Value -> Env -> Env
insertEnv name value (Env env) = Env $ Map.insert name (return value) env

unionEnv :: Defs -> Env -> Env
unionEnv [] env = env
unionEnv ((name, value) : defs) env =
    unionEnv defs $ insertEnvLazy name value env

insertEnvLazy :: String -> EvalResult Value -> Env -> Env
insertEnvLazy name value (Env env) = Env $ Map.insert name value env

lookupEnv :: String -> Env -> Maybe (EvalResult Value)
lookupEnv name (Env env) = Map.lookup name env

type EvalResult = ReaderT Env (Either Error)

evalBlock :: Env -> [Tree] -> EvalResult Defs
evalBlock env = evalBlock' env False Set.empty []

evalBlock' :: Env -> Bool -> Set String -> [Tree] -> [Tree] -> EvalResult Defs
evalBlock' env found errors []    [] = return []
evalBlock' env found errors defer [] = if found
    then evalBlock' env False Set.empty [] defer
    else throwError $ Undefined errors
evalBlock' env found errors defer (car : cdr) =
    let result = do
            defs  <- evalToDefine (env, car)
            defs' <- evalBlock' (unionEnv defs env) True errors defer cdr
            return $ defs ++ defs'
    in  catchError result $ \case
            Undefined names ->
                evalBlock' env found (Set.union names errors) (car : defer) cdr
            Error string -> throwError $ Error string

eval :: (Env, Tree) -> EvalResult Value
eval (env, (Symbol name)) = case lookupEnv name env of
    Just value -> value
    Nothing    -> do 
        globals <- ask 
        case lookupEnv name globals of
            Just value -> value
            Nothing -> throwError $ Undefined $ Set.singleton name
eval (env, (CharTree char)      ) = return $ CharValue char
eval (env, (StringTree string)  ) = return $ StringValue string
eval (env, (IntegerTree integer)) = return $ IntegerValue integer
eval (env, (Apply fn args)      ) = do
    f <- eval (env, fn)
    apply env f $ map (env, ) args

evalToDefine :: (Env, Tree) -> EvalResult Defs
evalToDefine tree = eval tree >>= \case
    (Define defs) -> return defs
    x -> throwError $ Error $ "Expected expression, found " ++ show x

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
apply env (Define defs) args =
    apply env (Function 1 $ \_ [expr] -> applyDef defs expr) args
apply env (Expr tree) args = do
    evArgs <- mapM evalToExpr args
    return $ Expr $ if null evArgs then tree else Apply tree evArgs
apply env x args = throwError $ Error $ "Expected function, found " ++ show x

applyDef :: Defs -> (Env, Tree) -> EvalResult Value
applyDef [] expr = eval expr
applyDef ((name, value) : defs) (env, tree) =
    applyDef defs (insertEnvLazy name value env, tree)
