{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

data Process
    = Impure (IO Value)
    | Do Process (Value -> EvalResult Process)

data Value
    = Function Int (Env -> [EvalResult Value] -> EvalResult Value)
    | Macro Int (Env -> [Tree] -> EvalResult Value)
    | Define Env
    | Expr Tree
    | CharValue Char
    | StringValue String
    | IntValue Integer
    | ProcessValue Process

data Error
    = Error String
    | Undefined (Set String)

instance Show Value where
    show (Function n f        ) = "<function>"
    show (Macro    n f        ) = "<macro>"
    show (Expr         t      ) = "'" ++ show t
    show (CharValue    c      ) = show c
    show (StringValue  s      ) = show s
    show (IntValue     i      ) = show i
    show (Define       (Env e)) = "{" ++ unwords (Map.keys e) ++ "}"
    show (ProcessValue p      ) = "<process>"

instance Show Error where
    show (Error     string) = "Error: " ++ string
    show (Undefined ns    ) = "Undefined: " ++ unwords (Set.toList ns)

newtype Env = Env (Map String (EvalResult Value))
    deriving (Semigroup)

insertEnv :: String -> Value -> Env -> Env
insertEnv name value (Env env) = Env $ Map.insert name (return value) env

unionEnv :: Env -> Env -> Env
unionEnv (Env a) (Env b) = Env $ Map.union a b

insertEnvLazy :: String -> EvalResult Value -> Env -> Env
insertEnvLazy name value (Env env) = Env $ Map.insert name value env

lookupEnv :: String -> Env -> Maybe (EvalResult Value)
lookupEnv name (Env env) = Map.lookup name env

type EvalResult = ReaderT Env (Except Error)

evalBlock :: Env -> [Tree] -> EvalResult Env
evalBlock env = evalBlock' env False Set.empty []

evalBlock' :: Env -> Bool -> Set String -> [Tree] -> [Tree] -> EvalResult Env
evalBlock' env found errors []    [] = return $ Env Map.empty
evalBlock' env found errors defer [] = if found
    then evalBlock' env False Set.empty [] defer
    else throwError $ Undefined errors
evalBlock' env found errors defer (car : cdr) =
    let result = do
            defs  <- asDefine $ eval (env, car)
            defs' <- evalBlock' (unionEnv defs env) True errors defer cdr
            return $ unionEnv defs defs'
    in  catchError result $ \case
            Undefined names ->
                evalBlock' env found (Set.union names errors) (car : defer) cdr
            Error string -> throwError $ Error string

eval :: (Env, Tree) -> EvalResult Value
eval (env, (SymbolTree name)) = case lookupEnv name env of
    Just value -> value
    Nothing    -> do
        globals <- ask
        case lookupEnv name globals of
            Just value -> value
            Nothing    -> throwError $ Undefined $ Set.singleton name
eval (env, (IntTree integer)      ) = return $ IntValue integer
eval (env, (CharTree char)        ) = return $ CharValue char
eval (env, (StringTree string)    ) = return $ StringValue string
eval (env, (ApplyTree [])         ) = return nil
eval (env, (ApplyTree (fn : args))) = do
    f <- eval (env, fn)
    apply env f $ map (env, ) args

asDefine :: EvalResult Value -> EvalResult Env
asDefine tree = tree >>= \case
    (Define defs) -> return defs
    x -> throwError $ Error $ "Expected expression, found " ++ show x

asExpr :: EvalResult Value -> EvalResult Tree
asExpr tree = tree >>= \case
    (Expr tree) -> return tree
    x           -> throwError $ Error $ "Expected expression, found " ++ show x

asSymbol :: EvalResult Value -> EvalResult String
asSymbol tree = asExpr tree >>= \case
    (SymbolTree s) -> return s
    x              -> throwError $ Error $ "Expected symbol, found " ++ show x

asChar :: EvalResult Value -> EvalResult Char
asChar tree = tree >>= \case
    (CharValue char) -> return char
    x -> throwError $ Error $ "Expected character, found " ++ show x

asString :: EvalResult Value -> EvalResult String
asString tree = tree >>= \case
    (StringValue string) -> return string
    x -> throwError $ Error $ "Expected string, found " ++ show x

asInteger :: EvalResult Value -> EvalResult Integer
asInteger tree = tree >>= \case
    (IntValue i) -> return i
    x            -> throwError $ Error $ "Expected integer, found " ++ show x

asProcess :: EvalResult Value -> EvalResult Process
asProcess tree = tree >>= \case
    (ProcessValue p) -> return p
    x -> throwError $ Error $ "Expected process, found " ++ show x

apply :: Env -> Value -> [(Env, Tree)] -> EvalResult Value
apply env (Macro n f) args =
    let argsLength = length args
    in  if argsLength < n
            then return $ Macro (n - argsLength) $ \env args' ->
                apply env (Macro n f) (args ++ map (env, ) args')
            else if argsLength > n
                then apply env (Macro n f) (take n args)
                    >>= \f -> apply env f (drop n args)
                else f env $ map snd args
apply env (Define defs) args =
    apply env (Macro 1 $ \env [expr] -> eval (unionEnv env defs, expr)) args
apply env (Expr tree) args = do
    evArgs <- mapM (asExpr . eval) args
    return $ Expr $ if null evArgs then tree else ApplyTree $ tree : evArgs
apply env x args = applyFn env x $ map eval args

applyFn :: Env -> Value -> [EvalResult Value] -> EvalResult Value
applyFn env (Function n f) args =
    let argsLength = length args
    in  if argsLength < n
            then return $ Function (n - argsLength) $ \env args' ->
                applyFn env (Function n f) (args ++ args')
            else if argsLength > n
                then applyFn env (Function n f) (take n args)
                    >>= \v -> applyFn env v (drop n args)
                else f env args
applyFn env x args = throwError $ Error $ "Expected function, found " ++ show x

nil :: Value
nil = Expr $ ApplyTree []
