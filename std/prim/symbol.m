#(Module for using M symbols)
(defmodule symbol {
  #(Tests if two symbols are equal)
  (def eq eq@symbol)

  #(The inverse of eq)
  (defn (neq a b) 
    ((bool not) (eq a b)))
})
