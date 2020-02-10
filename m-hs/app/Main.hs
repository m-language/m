import           Parse
import           Tree
import           Command
import           Eval
import           Special

import           Data.HashMap                   ( Map )
import qualified Data.HashMap                  as Map
import           Data.Functor
import           Data.Char
import           Data.List

import           Text.ParserCombinators.Parsec
                                         hiding ( try )

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict
import           System.Console.Haskeline
import           System.IO
import           System.Environment
import           IO

loop :: InputT (StateT Env IO) ()
loop = do
    line <- getInputLine "M> "
    case line of
        Nothing    -> loop
        Just input -> do
            env    <- lift get
            tryEnv <- liftIO
                (try $ process input env :: IO (Either SomeException Env))
            case tryEnv of
                Left  e      -> liftIO (print e) >> loop
                Right newEnv -> lift (put newEnv) >> loop

process :: String -> Env -> IO Env
process (':' : line) =
    let (command, rest) = break isSpace line in runCommand command rest
process line = runEvalCommand line

mComplete :: CompletionFunc (StateT Env IO)
mComplete = completeWord Nothing " \t()\"\'" completions
  where
    completions :: String -> (StateT Env IO) [Completion]
    completions symbol = get <&> \(Env env) ->
        map simpleCompletion $ sort $ filter (isPrefixOf symbol) (Map.keys env)

main :: IO ()
main = do
    args <- getArgs
    env  <- runLoadCommand (unwords args) (special <> io)
    evalStateT (runInputT (setComplete mComplete defaultSettings) loop) env
