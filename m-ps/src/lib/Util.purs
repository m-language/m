module Util where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Maybe.Trans (MaybeT, lift, runMaybeT)
import Control.Plus (empty)
import Data.Either (Either(..), either)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception (throw)
import Node.FS.Stats (isDirectory, isFile)
import Node.FS.Sync (readdir, stat)
import Node.Path (FilePath)
import Node.Path as Path
  
except :: forall m e a. MonadError e m => e -> Maybe a -> m a
except e v = maybe (throwError e) pure v

exceptEither :: forall m e a. MonadError e m => Either e a -> m a
exceptEither = either (throwError) (pure)


words :: String -> Array String
words = split $ Pattern " "

doesDirectoryExist :: FilePath -> Effect Boolean
doesDirectoryExist path = do
  stats <- stat path
  pure $ isDirectory stats

doesFileExist :: FilePath -> Effect Boolean
doesFileExist path = do
  stats <- stat path
  pure $ isFile stats


listDirectory :: FilePath -> Effect (List FilePath)
listDirectory root = readdir root <#> (\paths -> List.fromFoldable $ paths <#> \path -> Path.concat [root, path]) >>= \paths -> do
  files <- List.filterM doesFileExist paths
  directories <- List.filterM doesDirectoryExist paths
  subFiles <- traverse listDirectory directories <#> List.concat
  pure $ List.concat $ List.fromFoldable [files, subFiles]

listFilesWithExtension :: String -> FilePath -> Effect (List FilePath)
listFilesWithExtension end root = (listDirectory root <#> List.filter \file -> Path.extname file == end) >>= (traverse (Path.resolve []))

printEither :: forall m a b. Show a => MonadEffect m => Either a b -> MaybeT m b
printEither error = case error of
  Left e -> (liftEffect $ logShow e) *> empty
  Right b -> pure b

throwEffect :: forall e a. Show e => Either e a -> Effect a
throwEffect (Left e) = throw $ show e
throwEffect (Right a) = pure a 

runDefault :: forall m a. Monad m => a -> MaybeT m a -> m a
runDefault a maybeT = runMaybeT maybeT <#> fromMaybe a