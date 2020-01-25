#(Module for using M symbols)
(defpackage (symbol bool) [eq concat neq] {
  #(The inverse of eq)
  (defn (neq a b) (not@bool (eq a b)))
})
