module IO where

import           Eval
import           Tree
import           Special
import qualified Data.HashMap                  as Map
import           Data.Functor

io :: Env
io = Env $ Map.fromList
    [ function "stdout" 1 stdout'
    , value "stdin"   stdin'
    , value "newline" newline'
    ]

stdout' :: Env -> [EvalResult Value] -> EvalResult Value
stdout' env [char] =
    asChar char <&> \c -> ProcessValue $ Impure $ putChar c <&> const nil

stdin' :: Value
stdin' = ProcessValue $ Impure $ CharValue <$> getChar

newline' :: Value
newline' = CharValue '\n'
