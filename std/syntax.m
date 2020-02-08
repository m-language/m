#(Generic definitions which are standard enough to leave at the top level)

#(Macro for defining macros)
(def defm
  (fm [signature expr]
    (case@expr signature
      [name] ((quote def) name expr)
      (error "Nil signature")
      [name args] ((quote def) name ((quote fm) args expr)))))

#(Macro for defining functions)
(defm (defn signature expr)
  (case@expr signature
    [name] ((quote def) name expr)
    (error "Nil signature")
    [name args] ((quote def) name ((quote fn) args expr))))

#(Macro for defining function macros)
(defm (defnm signature args expr)
  ((quote defn) signature
    ((quote fm) args expr)))

#(The identity function)
(defn (id x) x)

#(Creates a function which always returns the same result)
(defn (const x) 
  (fn y x))

#(Composes two functions)
(defn (compose f g)
  (fn x (f (g x))))

#(Flips the order of a function's arguments)
(defn (flip f)
  (fn [x y] (f y x)))

#(Applies a function to an argument)
(defn (apply f x)
  (f x))
