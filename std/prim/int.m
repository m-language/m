#(Module for using M integers)
(defmodule int {
  #(Adds two integers)
  (def add add@int)

  #(Subtracts two integers)
  (def sub sub@int)

  #(Multiplies two integers)
  (def mul mul@int)

  #(Divides two integers)
  (def div div@int)

  #(Tests if an integer is less than another)
  (def lt lt@int)

  #(Tests if an integer is greater than another)
  (def gt gt@int)

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
