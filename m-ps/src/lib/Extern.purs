module Extern where

import Prelude

import Control.Monad.Except (ExceptT, mapExceptT, throwError, withExceptT)
import Data.Array (fold)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d1)
import Data.Vec as Vec
import Effect (Effect)
import Effect.Class (liftEffect)
import Eval (functionN, liftMarshall, liftResult)
import Eval.Types (Env(..), Error(..), EvalResult, Value(..), asString)
import Foreign (Foreign)
import Foreign.Index ((!))
import Foreign.Keys (keys)

foreign import externImpl :: String -> Foreign
foreign import require :: String -> Effect Foreign
foreign import merge :: Foreign -> Foreign -> Foreign
foreign import emptyObject :: Foreign
foreign import hasProperty :: Foreign -> String -> Boolean

data ExternError = Conflict (Set.Set String) | Generic String

instance showExternError :: Show ExternError where
  show (Conflict env) = "(Conflict " <> show env <> ")"
  show (Generic s) = show s

extern' :: (String -> EvalResult Foreign) -> Value
extern' impl = functionN d1 $ \env -> \symbolValue -> do
  symbol <- asString $ Vec.head symbolValue
  value <- impl symbol
  pure $ ExternValue $ value

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
  combinedKeys <- mapExceptT (unwrap >>> pure) $ withExceptT (Generic <<< show) $ keys combined <#> Set.fromFoldable
  pure $ Env $ Map.fromFoldable 
    [ Tuple "extern" $ extern' \key -> do
        if hasProperty combined key
          then liftMarshall $ liftResult $ combined ! key
          else throwError $ Error $ fold [ "Expected object with property "
                                         , key
                                         , ", got object with properties {"
                                         , joinWith "," $ map show $ Array.fromFoldable combinedKeys
                                         , "}" ]
    ]