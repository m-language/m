#(An implementation of either as a higher order function whose argument is
  applied to the left or right value)
(defmodule either [left right] {
  #(Creates a left either)
  (defn (left value)
    (fn [l r]
      (l value)))

  #(Creates a right either)
  (defn (right value)
    (fn [l r]
      (r value)))
})
