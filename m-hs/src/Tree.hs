{-# LANGUAGE FlexibleContexts #-}
module Tree where

import           Data.HashMap                   ( Map )

data Tree
    = Symbol String
    | CharTree Char
    | StringTree String
    | IntegerTree Integer
    | Apply Tree [Tree]
    deriving (Eq)

instance Show Tree where
    show (Symbol name  ) = name
    show (Apply fn args) = "(" ++ unwords (map show (fn : args)) ++ ")"
