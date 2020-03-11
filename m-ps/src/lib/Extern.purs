module Extern where

import Prelude

import Control.Monad.Except (ExceptT, mapExceptT, throwError, withExceptT)
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d1)
import Data.Vec as Vec
import Effect (Effect)
import Effect.Class (liftEffect)
import Eval (functionN, liftMarshall, liftResult)
import Eval.Types (Env(..), Value(..), asString)
import Foreign (Foreign, F)
import Foreign.Index ((!))
import Foreign.Keys (keys)

foreign import externImpl :: String -> Foreign
foreign import require :: String -> Effect Foreign
foreign import merge :: Foreign -> Foreign -> Foreign
foreign import emptyObject :: Foreign

extern' :: (String -> F Foreign) -> Value
extern' impl = functionN d1 $ \env -> \symbolValue -> do
  symbol <- asString $ Vec.head symbolValue
  value <- liftMarshall $ liftResult $ impl symbol
  pure $ ExternValue $ value

data ExternError = Conflict (Set.Set String) | Generic String

instance showExternError :: Show ExternError where
  show (Conflict env) = "(Conflict " <> show env <> ")"
  show (Generic s) = show s

mergeWithConflicts :: Foreign -> Foreign -> ExceptT ExternError Effect Foreign
mergeWithConflicts first second = do
  firstKeys <- mapExceptT (unwrap >>> pure) $ withExceptT (Generic <<< show) $ keys first <#> Set.fromFoldable
  secondKeys <- mapExceptT (unwrap >>> pure) $ withExceptT (Generic <<< show) $ keys second <#> Set.fromFoldable
  let intersection = Set.intersection firstKeys secondKeys
  if Set.size (intersection) /= 0
    then throwError $ Conflict intersection
    else pure $ merge first second

loadExternal :: Array String -> ExceptT ExternError Effect Env
loadExternal paths = do
  modules <- liftEffect $ traverse require paths
  combined <- foldM mergeWithConflicts emptyObject modules
  pure $ Env $ Map.fromFoldable 
    [ Tuple "extern" $ extern' \key -> combined ! key ]