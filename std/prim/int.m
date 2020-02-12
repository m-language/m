#(Module for using M integers)
(defmodule int {
  (import int-ops)

  #(The additive identity of integers)
  (def zero 0)

  #(The multiplicative identity of integers)
  (def one 1)

  #(Similar to div, but errors if the divisor is zero)
  (defn (div-unsafe x y)
    (div x y (error "Divisor is zero")))

  (import bool
    (defn (eq a b)
      (and (not (lt a b))
           (not (gt a b)))))

  #(Negates x)
  (defn (neg x) (sub 0 x))
})
