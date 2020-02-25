module IO where

import Data.Functor
import Data.List
import Effect
import Eval
import Prelude
import Special
import Tree
import Data.Map as Map

newtype Input
  = Input
  { getChar :: Effect Char
  , putChar :: Char -> Effect Unit
  }

io :: Partial => Input -> Env
io (Input i) =
  Env
    $ Map.fromFoldable
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
