module Tree where

import Prelude
import Data.Show
import Data.Semigroup
import Data.Eq
import Data.List
import Data.Array as Array
import Data.String.Common

data Tree
  = SymbolTree String
  | IntTree Int
  | CharTree Char
  | StringTree String
  | ApplyTree (List Tree)

derive instance eqTree :: Eq Tree

instance showTree :: Show Tree where
  show (SymbolTree name) = name
  show (IntTree int) = show int
  show (CharTree char) = show char
  show (StringTree string) = show string
  show (ApplyTree args) = "(" <> (joinWith " " (Array.fromFoldable (map show args))) <> ")"
