module IO where

import Data.List (List(..), (:))
import Data.Map as Map
import Effect (Effect)
import Eval (Env(..), EvalResult, Process(..), Value(..), asChar, nil)
import Prelude (Unit, const, ($), (<#>), (<$>))
import Special (function, value)

newtype Input
  = Input
  { getChar :: Effect Char
  , putChar :: Char -> Effect Unit
  }

io :: Partial => Input -> Env
io (Input i) = Env $ Map.fromFoldable
    [ function "stdout" 1 (stdout' i.putChar)
    , value "stdin" (stdin' i.getChar)
    , value "newline" newline'
    ]

stdout' :: Partial => (Char -> Effect Unit) -> Env -> List (EvalResult Value) -> EvalResult Value
stdout' putChar env (char : Nil) = asChar char <#> \c -> ProcessValue $ Impure $ putChar c <#> const nil

stdin' :: Effect Char -> Value
stdin' getChar = ProcessValue $ Impure $ CharValue <$> getChar

newline' :: Value
newline' = CharValue '\n'
