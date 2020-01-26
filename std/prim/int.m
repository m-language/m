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

  (def lt lt@int)
  (def gt gt@int)

  #(Similar to div, but errors if the divisor is zero)
  (defn (div-unsafe x y)
    (div x y (error "Divisor is zero")))

  #(Negates x)
  (defn (neg x) (sub 0 x))
})
