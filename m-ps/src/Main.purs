module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Node.Process (argv)
import Data.Array as Array
import Repl as Repl
import Repl.Node as NodeRepl

main :: Effect Unit
main = do
  args <- argv <#> Array.drop 2
  NodeRepl.evalNodeRepl $ do
    for_ args Repl.load
    Repl.repl


  