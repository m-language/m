;;; Function.m
;;;
;;; Various higher order functions which have no external dependencies.

;; The identity function.
(def id (fn x x))

;; A function which always returns the same result.
(def const
  (fn x
    (fn _ x)))

;; Composes two functions.
(def compose
  (fn f g
    (fn x
      (f (g x)))))

;; Swaps the order of a function's arguments.
(def swap
  (fn f
    (fn x y
      (f y x))))

;; Applies a function to an argument.
(def apply
  (fn f x
    (f x)))

;; The inverse of apply.
(def with (swap apply))