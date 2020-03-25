module Tree where

import Data.Array as Array
import Data.BigInt (BigInt, toString)
import Data.List (List)
import Data.String (joinWith)
import Prelude (class Eq, class Show, map, show, (<>), ($))

data Tree
  = SymbolTree String
  | IntTree BigInt
  | CharTree Char
  | StringTree String
  | ApplyTree Tree Tree
  | ListTree (List Tree)

derive instance eqTree :: Eq Tree

instance showTree :: Show Tree where
  show (SymbolTree name) = name
  show (IntTree int) = toString int
  show (CharTree char) = show char
  show (StringTree string) = show string
  show (ApplyTree fn arg) = "(" <> show fn <> " " <> show arg <> ")"
  show (ListTree trees) = "[" <> (joinWith " " $ Array.fromFoldable $ map show trees) <> "]"
