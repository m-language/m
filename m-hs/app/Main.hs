import           Parse
import           Tree
import           Command
import           Eval
import           Special

import           Data.HashMap                   ( Map )
import qualified Data.HashMap                  as Map
import           Data.Functor
import           Data.Char

import           Text.ParserCombinators.Parsec
                                         hiding ( try )

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           System.Console.Haskeline
import           System.IO
import           System.Environment
import           IO

loop :: Env -> InputT IO ()
loop env = do
    line <- getInputLine "M> "
    case line of
        Nothing    -> loop env
        Just input -> do
            tryEnv <- liftIO
                (try $ process input env :: IO (Either SomeException Env))
            case tryEnv of
                Left  e      -> liftIO (print e) >> loop env
                Right newEnv -> loop newEnv

process :: String -> Env -> IO Env
process (':' : line) =
    let (command, rest) = break isSpace line in runCommand command rest
process line = runEvalCommand line

main :: IO ()
main = do
    args <- getArgs
    env  <- runLoadCommand (unwords args) (special <> io)
    runInputT defaultSettings $ loop env
