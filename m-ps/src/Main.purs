module Main where

import Prelude

import Command (parseAndEvaluate, runCommand, runEvalCommand, runLoadCommand)
import Control.Monad.Cont (ContT(..), lift, runContT)
import Control.Monad.State (StateT, put, runStateT)
import Data.Array as Array
import Data.Char.Unicode (isSpace)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (length, drop)
import Data.String.CodeUnits (charAt, singleton, takeWhile)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Eval.Types (Env)
import IO (Input(..), io)
import Node.Encoding (Encoding(..))
import Node.Process (argv, stdout)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)
import Special (special)

foreign import readInputCharImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

readInputChar :: Effect (Maybe Char)
readInputChar = readInputCharImpl Just Nothing <#> \m -> m >>= charAt 0

break :: String -> Tuple String String
break s = 
  let prefix = takeWhile (not isSpace) s
      postfix = drop (length prefix) s
  in  Tuple prefix postfix

more :: Interface -> ContT Unit Effect String 
more interface = ContT $ \k -> question ".. " k interface

input :: Interface -> ContT Unit Effect String
input interface = ContT \k -> question "M> " k interface

repl :: Interface -> StateT Env (ContT Unit Effect) Unit
repl interface = do
  userInput <- lift $ input interface
  env <- parseAndEvaluate userInput (more interface)
  put env
  repl interface

process :: String -> Env -> Effect Env
process line env
  | length line == 0 = pure env
  | charAt 0 line == Just ':' =
    let Tuple command rest = break $ drop 1 line
    in  runCommand command rest env
  | otherwise = runEvalCommand line env

basicIO :: Input
basicIO = Input
    { getChar: readInputChar
    , putChar: putChar
    }
  where
    putChar = \char -> void $ Stream.writeString stdout UTF8 (singleton char) (pure unit)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  args <- argv <#> Array.drop 2
  let initialEnv = unsafePartial (special <> io basicIO)
  env <- runLoadCommand (List.fromFoldable args) initialEnv
  runContT (runStateT (repl interface) env) (pure >>> void)