#(A simple module system which supports importing and exporting, where 
  (defmodule) defines a module and (import) imports it)

#(Defins a module given a signature, a list of imports, and a list of 
  definitions)
(defm (defmodule signature defs)
  ((quote defn) signature
    ((quote block) defs)))

#(Imports a list of modules)
(defm (import imports)
  (case@expr imports
    [name] name
    id
    [car cdr] ((quote compose) car ((quote import) cdr))))
