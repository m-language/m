module Repl.Web where

import Prelude

import Control.Monad.State (StateT(..))
import Effect (Effect)
import Eval.Types (Env)

newtype WebRepl = WebRepl (StateT Env Effect Unit)