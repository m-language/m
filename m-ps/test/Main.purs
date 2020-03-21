module Test.Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trampoline (runTrampoline)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Eval (eval)
import Eval.Types (asString)
import Partial.Unsafe (unsafePartial)
import Special (special)
import Test.Identity (Identity(..))
import Test.QuickCheck (Result, quickCheck', (<?>))
import Tree (Tree(..))

main :: Effect Unit
main = unsafePartial $ quickCheck' 1000 testIdentity

testIdentity :: Partial => Identity -> Result
testIdentity (Identity identity) = 
  let tree = StringTree "Hello"
      identicalTree = identity tree
      result = runTrampoline $ runExceptT $ runReaderT (asString $ eval $ Tuple mempty tree) special
      identicalResult = runTrampoline $ runExceptT $ runReaderT (asString $ eval $ Tuple mempty identicalTree) special
  in result == identicalResult <?> show identicalTree
