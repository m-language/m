#(Generic definitions which are standard enough to leave at the top level)

#(Macro for defining macros)
(def deffm
  (fm [name args expr]
    ((quote def) name
      ((quote fm) args expr))))

#(Macro for defining functions)
(deffm deffn [name args expr]
  ((quote def) name
    ((quote fn) args expr)))

#(The function of an expression)
(deffn expr-function expr
  ((expr-ops case) expr expr
    (fn [f a] (expr-function f))
    (error "Expected apply, found list")))

#(The arguments of an expression)
(def expr-args (expr-args-acc []))

(deffn expr-args-acc [acc expr]
  ((expr-ops case) expr acc
    (fn [f a] (expr-args-acc [a acc] f))
    (error "Expected apply, found list")))

#(Syntax sugar for deffm)
(deffm defm [signature expr]
  ((quote def) (expr-function signature)
    ((quote fm) (expr-args signature) expr)))

#(Syntax sugar for deffn)
(defm (defn signature expr)
  ((quote def) (expr-function signature)
    ((quote fn) (expr-args signature) expr)))

#(Macro for letting a value be the result of a continuation)
(defm (let-cont names value expr)
  (value ((quote fn) names expr)))

#(The identity function)
(defn (id x) x)

#(Creates a function which always returns the same result)
(defn (const x y) x)

#(Composes two functions)
(defn (compose f g x)
  (f (g x)))

#(Flips the order of a function's arguments)
(defn (flip f x y)
  (f y x))

#(Applies a function to an argument)
(defn (apply f x)
  (f x))

#(Chains a list of applications)
(defm (chain functions)
  ((expr case) functions
    (error "Expected list, found symbol")
    (error "Expected list, found apply")
    (fn list
      (list
        (quote id)
        (fn [car cdr]
          ((quote compose) car
            ((quote chain) cdr)))))))