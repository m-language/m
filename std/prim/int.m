#(Module for using M integers)
(defpackage (int bool) [add sub mul div lt gt eq div-unsafe neg] {
  #(Similar to div, but errors if the divisor is zero)
  (defn (div-unsafe x y)
    (div x y (error "Divisor is zero")))
 
  #(Tests for integer equality)
  (defn (eq x y)
    (lt x y false
      (gt x y false true)))

  #(Negates x)
  (defn (neg x) (sub 0 x))
})
