module Main where

import Command

import Data.Monoid (mempty)
import Data.Ord ((>))
import Control.Monad.State (StateT, evalStateT, execStateT, get, put)
import Data.Array as Array
import Data.Array.NonEmpty (elemLastIndex)
import Data.Char.Unicode (isSpace)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, length, drop)
import Data.String.CodeUnits (singleton, takeWhile)
import Data.String.Unsafe (charAt)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error) as Exception
import Effect.Exception (throwException, try)
import Eval (Env)
import IO (Input(..), io)
import Node.Encoding (Encoding(..))
import Node.Process (argv, stdout, stdin)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, not, otherwise, pure, show, unit, void, when, ($), (<#>), (<>), (==))
import Special (special)

unwords :: Array String -> String
unwords = joinWith " "

break :: String -> Tuple String String
break s = 
  let prefix = takeWhile (not isSpace) s
      postfix = drop (length prefix) s
  in  Tuple prefix postfix

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
          Left e -> liftEffect $ log $ show e
          Right newEnv -> put newEnv

      handleLine :: Env -> String -> Effect Unit
      handleLine old line = do
        newState <- execStateT (runLine line) old
        evalStateT (loop interface) newState

process :: String -> Env -> Effect Env
process line env
  | charAt 0 line == ':' =
    let Tuple command rest = break $ drop 1 line
    in  runCommand command rest env
  | otherwise = runEvalCommand line env

-- mComplete :: CompletionFunc (StateT Env Effect)
-- mComplete = completeWord Nothing " \t()\"\'" completions
--   where
--   completions :: String -> (StateT Env Effect) (List Completion)
--   completions symbol =
--     get
--       <&> \(Env env) ->
--           map simpleCompletion $ sort $ filter (isPrefixOf symbol) (Map.keys env)
-- FIXME: input
basicIO :: Input
basicIO = Input
    { getChar: Stream.readString stdin (Just 1) UTF8 <#> fromMaybe "" <#> charAt 0
    , putChar: \c -> void $ Stream.writeString stdout UTF8 (singleton c) (pure unit)
    }

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  args <- argv <#> Array.drop 2
  let initialEnv = ((unsafePartial special) <> (unsafePartial (io basicIO)))
  env <- if (Array.length args) > 0 then runLoadCommand (unwords args) initialEnv else pure initialEnv
  evalStateT (loop interface) env
