module Main where

import Command

import Control.Monad((>>=))
import Control.Monad.State (StateT, evalStateT, execStateT, get, put)
import Data.Array as Array
import Data.Char.Unicode (isSpace)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length, drop)
import Data.String.CodeUnits (charAt, singleton, takeWhile)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (try)
import Eval (Env)
import IO (Input(..), io)
import Node.Encoding (Encoding(..))
import Node.Process (argv, stdout)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, not, otherwise, pure, show, unit, void, ($), (<#>), (<>), (==))
import Special (special)

foreign import readInputCharImpl :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

readInputChar :: Effect (Maybe Char)
readInputChar = readInputCharImpl Just Nothing <#> \m -> m >>= charAt 0

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
        tryEnv <- liftEffect $ try $ process line env'
        case tryEnv of
          Left e -> liftEffect $ log $ show e
          Right newEnv -> put newEnv

      handleLine :: Env -> String -> Effect Unit
      handleLine env line = do
        env' <- execStateT (runLine line) env
        evalStateT (loop interface) env'

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
    , putChar: \c -> void $ Stream.writeString stdout UTF8 (singleton c) (pure unit)
    }

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  args <- argv <#> Array.drop 2
  let initialEnv = unsafePartial (special <> (io basicIO))
  env <- runLoadCommand args initialEnv
  evalStateT (loop interface) env
