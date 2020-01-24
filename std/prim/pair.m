#(An implementation of pairs as a function from a boolean to a value, where
  (pair true) is the first value of the pair and (pair false) is the second 
  value of the pair)
(defmodule (pair bool) [pair first second] {
  #(Creates a pair of two values)
  (defn (pair first second)
    (fn f
      (f first second)))
  
  #(The first value of a pair)
  (defn (first pair)
    (pair true))
  
  #(The second value of a pair)
  (defn (second pair)
    (pair false))
})
