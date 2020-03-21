#(A simple module system which supports importing and exporting, where 
  (defmodule) defines a module and (import) imports it)

#(Defins a module given a signature and a list of definitions)
(defm (defmodule signature defs)
  ((quote defn) signature
    ((quote block) defs)))

#(Imports a list of modules)
(defm (import imports)
  ((expr-ops case) imports
    imports
    ((quote block) {})
    (fn [car cdr] 
      ((quote block) {
        car
        ((quote import) cdr)
      }))))

#(Defines a value which is parameterized over any number of modules)
(defm (def-generic modules name expr)
  ((quote defn) (name modules)
    ((quote def) name (name modules)
      ((quote import) modules expr))))

#(Defines a function which is parameterized over any number of modules)
(defm (defn-generic modules signature expr)
  ((expr-ops case) signature
    ((quote def-generic) signature expr)
    (error "Nil signature")
    (fn [name args] 
      ((quote def-generic) modules name 
        ((quote fn) args expr)))))
