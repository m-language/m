module Tree where

import Data.Array as Array
import Data.BigInt (BigInt, toString)
import Data.List (List)
import Data.String.Common (joinWith)
import Prelude (class Eq, class Show, map, show, (<>))

data Tree
  = SymbolTree String
  | IntTree BigInt
  | CharTree Char
  | StringTree String
  | ApplyTree (List Tree)

derive instance eqTree :: Eq Tree

instance showTree :: Show Tree where
  show (SymbolTree name) = name
  show (IntTree int) = toString int
  show (CharTree char) = show char
  show (StringTree string) = show string
  show (ApplyTree args) = "(" <> (joinWith " " (Array.fromFoldable (map show args))) <> ")"
