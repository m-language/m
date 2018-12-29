;; The identity function.
(def id
  (lambda x x))

;; A function which always returns the same result.
(def const
  (lambda x
    (lambda "" x)))

;; Composes two functions.
(def compose
  (lambda f
    (lambda g
      (lambda x
        (f (g x))))))

;; Swaps the order of a function's arguments.
(def swap
  (lambda f
    (lambda x
      (lambda y
        (f y x)))))

;; Applies a function to an argument.
(def apply
  (lambda f
    (lambda x
      (f x))))

;; The inverse of [apply].
(def with (swap apply))