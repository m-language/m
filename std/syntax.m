#(Generic definitions which are standard enough to leave at the top level)

#(Macro for defining macros)
(def defm
  (fm [signature expr]
    ((expr-ops case) signature
      ((quote def) signature expr)
      (error "Nil signature")
      (fn [name args] 
        ((quote def) name 
          ((quote fm) args expr))))))

#(Macro for defining functions)
(defm (defn signature expr)
  ((expr-ops case) signature
    ((quote def) signature expr)
    (error "Nil signature")
    (fn [name args] 
      ((quote def) name 
        ((quote fn) args expr)))))

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