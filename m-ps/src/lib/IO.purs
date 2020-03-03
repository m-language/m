module IO where

import Control.Monad ((>>=), pure)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error, throwException)
import Eval (Env(..), EvalResult, Process(..), Value(..), asChar, nil)
import Prelude (Unit, const, ($), (<#>), (<$>))
import Special (function, value)

newtype Input
  = Input
  { getChar :: Effect (Maybe Char)
  , putChar :: Char -> Effect Unit
  }

io :: Partial => Input -> Env
io (Input input) = Env $ Map.fromFoldable
    [ function "stdout" 1 (stdout' input.putChar)
    , value "stdin" (stdin' $ input.getChar >>= \m -> case m of
      Nothing -> throwException $ error "EOF"
      Just char -> pure char)
    , value "newline" newline'
    ]

stdout' :: Partial => (Char -> Effect Unit) -> Env -> List (EvalResult Value) -> EvalResult Value
stdout' putChar env (char : Nil) = asChar char <#> \c -> ProcessValue $ Impure $ putChar c <#> const nil

stdin' :: Effect Char -> Value
stdin' getChar = ProcessValue $ Impure $ CharValue <$> getChar

newline' :: Value
newline' = CharValue '\n'
