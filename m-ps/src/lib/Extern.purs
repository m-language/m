module Extern where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, mapExceptT, withExceptT)
import Control.Monad.Trampoline (done)
import Control.Monad.Trans.Class (lift)
import Data.Array (index, length)
import Data.Array as Array
import Data.BigInt (fromInt, fromNumber, toNumber)
import Data.Int as Int
import Data.List.Lazy.NonEmpty (head, singleton)
import Data.List.Lazy.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d1)
import Data.Vec as Vec
import Eval (Env(..), Error(..), EvalResult, Value(..), asInteger, asString)
import Foreign (F, Foreign, MultipleErrors, readArray, readBoolean, readNumber, readString, typeOf)
import Special (except, functionN, true', false', mkPair)

foreign import externImpl :: String -> Foreign

data MarshallError = InvalidBigInt Number | Multiple MultipleErrors | Generic String
type MarshallResult a = Except (NonEmptyList MarshallError) a

liftResult :: forall a. F a -> MarshallResult a
liftResult = withExceptT $ Multiple >>> singleton

liftMarshall :: forall a. MarshallResult a -> EvalResult a
liftMarshall = lift <<< mapExceptT (unwrap >>> done) <<< withExceptT (map mError >>> head)
  where
    mError :: MarshallError -> Error
    mError (Generic s) = Error s
    mError (Multiple errors) = Error $ joinWith ", " $ Array.fromFoldable $ map show errors
    mError (InvalidBigInt number) = Error $ "Expected foreign integer, got " <> show number

mkObject :: Map String Value -> Value
mkObject kv = functionN d1 $ \env -> \keySymbol -> do
  key <- asString $ Vec.head keySymbol
  except (Error $ "Could not find key " <> show key <> "in object") $ Map.lookup key kv

marshallInt :: Foreign -> MarshallResult Value
marshallInt fv = do
  n <- liftResult $ readNumber fv
  except (singleton $ InvalidBigInt n) (fromNumber n <#> IntValue)

marshallString :: Foreign -> MarshallResult Value
marshallString fv = liftResult $ readString fv <#> StringValue

marshallArray :: Foreign -> MarshallResult Value
marshallArray fv = do
  fa <- liftResult $ readArray fv
  mkArray fa
    where
      mkArray :: Array Foreign -> MarshallResult Value
      mkArray fa = traverse marshall fa <#> \elements -> 
        mkPair (IntValue $ fromInt $ length elements) $ functionN d1 \_ -> \arrayIndex -> do
          n <- asInteger (Vec.head arrayIndex) <#> toNumber
          let result = Int.fromNumber n >>= index elements
          except (Error $ "Expected number [0, " <> (show $ length elements) <> "), found " <> show n) result

marshallBoolean :: Foreign -> MarshallResult Value
marshallBoolean fv = liftResult $ readBoolean fv <#> \fb -> if fb then true' else false'

marshallObject :: Foreign -> MarshallResult Value
marshallObject fv = do
  let isObject = typeOf fv == "object"
  throwError $ singleton $ Generic "Unable to marshall object"

marshallFunction :: Foreign -> MarshallResult Value
marshallFunction fv = throwError $ singleton $ Generic "Unable to marshall function"

marshall :: Foreign -> MarshallResult Value
marshall fv = marshallInt fv
          <|> marshallString fv
          <|> marshallArray fv
          <|> marshallBoolean fv
          <|> marshallObject fv
          <|> marshallFunction fv

extern :: String -> EvalResult Value
extern = liftMarshall <<< marshall <<< externImpl

extern' :: Value
extern' = functionN d1 $ \env -> \symbolValue -> do
  symbol <- asString $ Vec.head symbolValue
  extern symbol

externEnv :: Env
externEnv = Env $ Map.fromFoldable [ Tuple "extern" extern' ]