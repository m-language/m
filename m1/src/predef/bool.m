;; The singleton truthy value, for which `(if true x y)` evaluates to x.
(def true ())

;; The singleton falsy value, for which `(if false x y)` evaluates to y.
(def false ())

;; True if both arguments are true.
(def and
  (lambda x
    (lambda y
      (if x (y) false))))

;; True if either argument is true.
(def or
  (lambda x
    (lambda y
      (if x true (y)))))

;; True if its argument is false.
(def not
  (lambda x
    (if x false true)))