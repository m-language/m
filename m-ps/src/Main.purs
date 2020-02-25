module Main where

import Command
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Tuple
import Data.Unit
import Effect
import Effect.Console
import Eval
import IO
import Node.ReadLine
import Parse
import Prelude
import Special
import Tree
import Data.Array as Array
import Data.Char.Unicode (isSpace)
import Data.Map (Map)
import Data.Map as Map
import Data.String (joinWith, length, drop)
import Data.String.CodeUnits (singleton, takeWhile)
import Data.String.Unsafe (charAt)
import Data.Tuple (Tuple)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception (Error) as Exception
import Effect.Exception (try)
import Node.Encoding (Encoding(..))
import Node.Process (argv, stdout, stdin)
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)

unwords :: Array String -> String
unwords = joinWith " "

break :: String -> Tuple String String
break s =
  let
    prefix = takeWhile (not isSpace) s
  in
    let
      postfix = drop (length prefix) s
    in
      (Tuple prefix postfix)

loop :: Interface -> StateT Env Effect Unit
loop interface = do
  current <- get
  liftEffect $ question "M> " (handleLine current) interface
  where
  runLine :: String -> StateT Env Effect Unit
  runLine line = do
    env' <- get
    tryEnv <- liftEffect (try $ process line env' :: Effect (Either Exception.Error Env))
    case tryEnv of
      Left e -> liftEffect (log (show e))
      Right newEnv -> (put newEnv)

  handleLine :: Env -> String -> Effect Unit
  handleLine old line = do
    newState <- execStateT (runLine line) old
    evalStateT (loop interface) newState

process :: String -> Env -> Effect Env
process line env =
  let
    without = drop 1 line
  in
    let
      (Tuple command rest) = break without
    in
      runCommand command rest env

process line env = runEvalCommand line env

-- mComplete :: CompletionFunc (StateT Env Effect)
-- mComplete = completeWord Nothing " \t()\"\'" completions
--   where
--   completions :: String -> (StateT Env Effect) (List Completion)
--   completions symbol =
--     get
--       <&> \(Env env) ->
--           map simpleCompletion $ sort $ filter (isPrefixOf symbol) (Map.keys env)
basicIO :: Input
basicIO =
  Input
    { -- FIXME input
    getChar: Stream.readString stdin (Just 1) UTF8 <#> fromMaybe "" <#> charAt 0
    , putChar: \c -> void $ Stream.writeString stdout UTF8 (singleton c) (pure unit)
    }

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  args <- argv <#> Array.drop 2
  env <- runLoadCommand (unwords args) ((unsafePartial special) <> (unsafePartial (io basicIO)))
  evalStateT (loop interface) env
