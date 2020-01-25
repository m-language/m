#(A simple package system which supports importing and exporting, where 
  (defpackage) defines a package and (import) uses it)

#(Evaluates an expression with a list of imported packages)
(defm (import packages expr)
  (case@expr packages
    [symbol] ((quote import-dyn) packages expr)
    [car] ((quote import) car expr)
    [car cdr]
      ((quote import-dyn) car
        ((quote import) cdr expr))))

#(Imports a package dynamically)
(defnm (import-dyn package) [expr]
  (package (import-dyn1 expr)))

(defn (import-dyn1 expr name exports)
  (case@expr exports
    [symbol] ((quote let) symbol (concat@symbol symbol name) expr)
    [car] ((quote let) car (concat@symbol car name) expr)
    [car cdr]
      ((quote let) car (concat@symbol car name)
        (import-dyn1 expr name cdr))))

#(Defines a package and evaluates a list of expressions in the package)
(defm (defpackage packages exports exprs)
  (case@expr packages
    [name]
      ((quote block) name {
        ((quote defn) (name (quote f))
          ((quote f)
            ((quote quote) name)
            ((quote quote) exports)))
        ((quote let) (quote def) ((quote package-def) name)
          ((quote block) name exprs))
      })
    [car] ((quote defpackage) car exports exprs)
    [car cdr]
      ((quote import) cdr
        ((quote defpackage) car exports exprs))))

#(Defines a variable in a package)
(defm (package-def package name expr)
  ((quote def1) (concat@symbol name package)
    ((quote import-dyn) package expr)))

#(An alias for def)
(def def1 def)
