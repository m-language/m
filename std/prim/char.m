#(Module for using M characters)
(defpackage (char bool) [eq neq] {
  #(The inverse of eq)
  (defn (neq a b) 
    (not (eq a b)))
})
