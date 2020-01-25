#(Module for using M symbols)
(defmodule (symbol bool) [eq concat neq] {
  #(The inverse of eq)
  (defn (neq a b) (not (eq a b)))
})
