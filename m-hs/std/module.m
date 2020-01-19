#(A simple module system which supports importing and exporting, where (module) 
  defines a module and (import) uses it)

#(Evaluates an expression with a list of imported modules)
(defm (import modules expr)
  (expr/case modules
    [symbol] ((quote import-dyn) modules expr)
    [car] ((quote import) car expr)
    [car cdr]
      ((quote import-dyn) car
        ((quote import) cdr expr))))

#(Imports a module dynamically)
(defn (import-dyn module)
  (fm expr
    (module (import-dyn' expr))))

(defn (import-dyn' expr name exports)
  (expr/case exports
    [symbol] ((quote let) symbol (symbol/concat name symbol) expr)
    [car] ((quote let) car (symbol/concat name car) expr)
    [car cdr]
      ((quote let) car (symbol/concat name car)
        (import-dyn' expr name cdr))))

#(Defines a module given a name and a list of exported symbols)
(defm (module name exports)
  ((quote def) name
    ((quote fn) (quote f)
      ((quote f)
        ((quote quote) name)
        ((quote quote) exports)))))

#(Defines a module and evaluates a list of expressions in the module)
(defm (defmodule modules exports exprs)
  (expr/case modules
    [name]
      ((quote block) name {
        ((quote module) name exports)
        ((quote let) (quote def) ((quote module-def) name)
          ((quote block) name exprs))
      })
    [car] ((quote defmodule) car exports exprs)
    [car cdr]
      ((quote import) cdr
        ((quote defmodule) car exports exprs))))

#(Defines a variable in a module)
(defm (module-def module name expr)
  ((quote def') (symbol/concat module name)
    ((quote import-dyn) module expr)))

#(An alias for def)
(def def' def)
