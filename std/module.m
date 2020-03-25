#(A simple module system which uses block)

#(Defines a module given a signature and a list of definitions)
(defm (defmodule signature defs)
  ((quote defn) signature
    ((quote block) defs)))

#(Imports a list of modules)
(defm (import imports)
  ((expr-ops case) imports imports
    (error "Expected list, found apply")
    (fn list
      (list
        ((quote block) {})
        (fn [car cdr]
          ((quote block) {
            car
            ((quote import) cdr)
          }))))))

#(Defines a value which is parameterized over a module)
(defm (def-generic module name expr)
  ((quote defn) (name module)
    ((quote def) name (name module)
      ((quote import) module expr))))

#(Defines a function which is parameterized over a module)
(defm (defn-generic module signature expr)
  ((quote def-generic) module (expr-function signature)
    ((quote fn) (expr-args signature) expr)))