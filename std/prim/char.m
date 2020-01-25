#(Module for using M characters)
(defmodule (char bool) [eq neq] {
  #(The inverse of eq)
  (defn (neq a b) (not (eq a b)))
})
