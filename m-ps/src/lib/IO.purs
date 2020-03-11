module IO where

import Control.Monad ((>>=), pure)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1, d1)
import Data.Vec (Vec)
import Data.Vec as Vec
import Effect (Effect)
import Effect.Exception (error, throwException)
import Eval (functionN)
import Eval.Types (Env(..), EvalResult, Process(..), Value(..), asChar, nil)
import Prelude (Unit, const, ($), (<#>), (<$>))

newtype Input
  = Input
  { getChar :: Effect (Maybe Char)
  , putChar :: Char -> Effect Unit
  }

io :: Input -> Env
io (Input input) = Env $ Map.fromFoldable
    [ Tuple "stdout" $ functionN d1 $ stdout' input.putChar
    , Tuple "stdin" $ stdin' $ input.getChar >>= \m -> 
        case m of
          Nothing -> throwException $ error "EOF"
          Just char -> pure char
    , Tuple "newline" newline'
    ]

stdout' :: (Char -> Effect Unit) -> Env -> Vec D1 (EvalResult Value) -> EvalResult Value
stdout' putChar env char = asChar (Vec.head char) <#> \c -> ProcessValue $ Impure $ putChar c <#> const nil

stdin' :: Effect Char -> Value
stdin' getChar = ProcessValue $ Impure $ CharValue <$> getChar

newline' :: Value
newline' = CharValue '\n'
