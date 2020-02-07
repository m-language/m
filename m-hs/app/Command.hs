{-# LANGUAGE LambdaCase #-}

module Command where

import           Parse
import           Tree
import           Eval

import           Text.ParserCombinators.Parsec
import           Control.Applicative
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Data.Functor
import           Data.Maybe
import           Data.Foldable
import           Control.Monad.Trans.Writer
import qualified Data.HashMap                  as Map
import           Data.Bifunctor
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Comonad
import           System.Directory
import           System.FilePath.Posix

runCommand :: String -> String -> Env -> IO Env
runCommand "parse"      rest env = runParseCommand rest env
runCommand "eval"       rest env = runEvalCommand rest env
runCommand "load-parse" rest env = runLoadParseCommand rest env
runCommand "load"       rest env = runLoadCommand rest env
runCommand name _ env =
    putStrLn ("Unrecognized command " ++ name) >> return env

runParseCommand :: String -> Env -> IO Env
runParseCommand rest env = runDefault env $ do
    tree <- printEither $ parseRepl rest
    lift $ print tree
    return env

runEvalCommand :: String -> Env -> IO Env
runEvalCommand rest env = runDefault env $ do
    tree  <- printEither $ parseRepl rest
    value <- printExcept $ runReaderT (eval (Env Map.empty, tree)) env
    case value of
        ProcessValue p    -> runProcess env p $> env
        Define       defs -> return $ unionEnv defs env
        x                 -> lift $ print value $> env

runLoadParseCommand :: String -> Env -> IO Env
runLoadParseCommand rest env = runDefault env $ do
    files <- lift $ parseFiles $ words rest
    trees <- printEither files
    forM_ trees (lift . print)
    return env

runLoadCommand :: String -> Env -> IO Env
runLoadCommand rest env = runDefault env $ do
    files <- lift $ parseFiles $ words rest
    trees <- printEither files
    defs  <- printExcept $ runReaderT (evalBlock (Env Map.empty) trees) env
    return $ unionEnv defs env

parseFile :: String -> IO (Either ParseError [Tree])
parseFile name = doesDirectoryExist name >>= \case
    True -> do
        names <- listDirectory name
        parseFiles $ map (name </>) names
    False -> do
        chars <- readFile name
        return $ parseProgram name chars

parseFiles :: [String] -> IO (Either ParseError [Tree])
parseFiles names = mapM parseFile names <&> (\f -> sequence f <&> concat)

printExcept :: (Show a) => Except a b -> MaybeT IO b
printExcept except = printEither $ extract $ runExceptT except

printEither :: (Show a) => Either a b -> MaybeT IO b
printEither error = case error of
    Left  e -> lift (print e) >> empty
    Right b -> return b

runDefault :: a -> MaybeT IO a -> IO a
runDefault a maybeT = runMaybeT maybeT <&> fromMaybe a

runProcess :: Env -> Process -> MaybeT IO (EvalResult Value)
runProcess env (Impure a) = lift $ return <$> a
runProcess env (Do process fn) = do
    value <- runProcess env process
    proc <- return $ value >>= fn
    process' <- printExcept $ runReaderT proc env
    runProcess env process'
