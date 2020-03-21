#(An implementation of either as a higher order function whose argument is
  applied to the left or right value)
(defmodule either {
  #(Creates a left either)
  (defn (left value l r) 
    (l value))
  
  #(Creates a right either)
  (defn (right value l r) 
    (r value))
})