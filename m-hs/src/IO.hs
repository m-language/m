module IO where

import           Eval
import           Tree
import qualified Data.HashMap                  as Map
import           Data.Functor

io :: Env
io = Env $ Map.fromList [("stdin", return stdin), entry "stdout" 1 stdout]
    where entry name i f = (name, return $ Function i f)

stdin :: Value
stdin = ProcessValue $ Impure $ CharValue <$> getChar

stdout :: Env -> [(Env, Tree)] -> EvalResult Value
stdout env [char] =
    evalToChar char <&> \c -> ProcessValue $ Impure $ putChar c <&> const Unit
