{-# LANGUAGE FlexibleContexts #-}
module Tree where

import           Data.HashMap                   ( Map )

data Tree
    = Symbol String
    | Apply Tree [Tree]
    deriving (Eq)

instance Show Tree where
    show (Symbol name  ) = name
    show (Apply fn args) = "(" ++ unwords (map show (fn : args)) ++ ")"
