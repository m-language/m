;; The singleton truthy value, for which `(if true x y)` evaluates to x.
(def true ())

;; The singleton falsy value, for which `(if false x y)` evaluates to y.
(def false ())

;; True if [x] and [y] are true.
(def and
  (lambda x
    (lambda y
      (if x (y ()) false))))

;; True if [x] or [y] is true.
(def or
  (lambda x
    (lambda y
      (if x true (y ())))))

;; True if [x] is false.
(def not
  (lambda x
    (if x false true)))