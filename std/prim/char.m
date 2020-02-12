#(Module for using M characters)
(defmodule char {
  (import char-ops)

  #(The newline character '\n')
  (def newline newline@char)

  #(The inverse of eq)
  (defn (neq a b)
    ((bool not) (eq a b)))
})
