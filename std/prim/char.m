#(Module for using M characters)
(defmodule char {
  #(Tests if two characters are equal)
  (def eq eq@char)

  #(The newline character '\n')
  (def newline newline@char)

  #(The inverse of eq)
  (defn (neq a b)
    ((bool not) (eq a b)))
})
