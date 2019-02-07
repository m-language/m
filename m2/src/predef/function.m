;;; Function.m
;;;
;;; Various higher order functions which have no external dependencies.

;; The identity function.
(def id (fn x x))

;; A function which always returns the same result.
(def const
  (fn x
    (fn "" x)))

;; Composes two functions.
(def compose
  (fn f
    (fn g
      (fn x
        (ap f (ap g x))))))

;; Swaps the order of a function's arguments.
(def swap
  (fn f
    (fn x
      (fn y
        (ap f y x)))))

;; Applies a function to an argument.
(def apply
  (fn f
    (fn x
      (ap f x))))

;; The inverse of apply.
(def with (ap swap apply))