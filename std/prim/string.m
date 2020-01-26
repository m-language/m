#(Module for using M strings)
(defmodule string {
  #(The length of a string)
  (def length length@string)

  #(Gets the character at an index in a string)
  (def get get@string)

  #(Similar to get, but errors if the index is out of bounds)
  (defn (get-unsafe string index) 
    (get string index (error "Index out of bounds")))
})
