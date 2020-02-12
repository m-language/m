#(A simple module system which supports importing and exporting, where 
  (defmodule) defines a module and (import) imports it)

#(Defins a module given a signature and a list of definitions)
(defm (defmodule signature defs)
  ((quote defn) signature
    ((quote block) defs)))

#(Imports a list of modules)
(defm (import imports)
  ((expr-ops case) imports
    [name] name
    ((quote block) {})
    [car cdr] ((quote block) {
      car
      ((quote import) cdr)
    })))
