module IO where

import           Eval
import           Tree
import qualified Data.HashMap                  as Map
import           Data.Functor

io :: Env
io = Env $ Map.fromList
    [ entry "stdout" 1 stdout'
    , ("stdin"  , return stdin')
    , ("newline", return newline')
    ]
    where entry name i f = (name, return $ Function i f)

stdout' :: Env -> [(Env, Tree)] -> EvalResult Value
stdout' env [char] =
    evalToChar char <&> \c -> ProcessValue $ Impure $ putChar c <&> const nil

stdin' :: Value
stdin' = ProcessValue $ Impure $ CharValue <$> getChar

newline' :: Value
newline' = CharValue '\n'
