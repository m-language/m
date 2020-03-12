module Main where

import Prelude

import Command (parseAndEvaluate, runCommand, runLoadCommand)
import Control.Monad.Cont (ContT(..), lift, runContT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Array as Array
import Data.Char.Unicode (isSpace)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (length, drop)
import Data.String.CodeUnits (charAt, singleton, takeWhile)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Eval.Types (Env)
import IO (Input(..), io)
import Node.Encoding (Encoding(..))
import Node.Process (argv, stdout)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)
import Special (special)

foreign import readInputCharImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

getChar :: Effect (Maybe Char)
getChar = readInputCharImpl Just Nothing <#> \m -> m >>= charAt 0

break :: String -> Tuple String String
break s = 
  let prefix = takeWhile (not isSpace) s
      postfix = drop (length prefix) s
  in  Tuple prefix postfix

more :: Interface -> ContT Unit Effect String 
more interface = ContT \cont -> question ".. " cont interface

input :: Interface -> ContT Unit Effect String
input interface = ContT \cont -> question "M> " cont interface

repl :: Interface -> StateT Env (ContT Unit Effect) Unit
repl interface = do
  userInput <- lift $ input interface
  env <- process interface userInput
  put env
  repl interface

process :: Interface -> String -> StateT Env (ContT Unit Effect) Env
process interface line
  | length line == 0 = get
  | charAt 0 line == Just ':' =
    let Tuple command rest = break $ drop 1 line
    in  get >>= \env -> liftEffect $ runCommand command rest env
  | otherwise = parseAndEvaluate line $ more interface

basicIO :: Input
basicIO = Input
    { getChar: getChar
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
  runContT (evalStateT (repl interface) env) pure