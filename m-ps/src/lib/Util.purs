module Util where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe, maybe)
  
except :: forall m e a. MonadError e m => e -> Maybe a -> m a
except e v = maybe (throwError e) pure v