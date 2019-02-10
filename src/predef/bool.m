;;; Bool.m
;;;
;;; An implementation of booleans which are encoded using two argument functions
;;; which ignore one argument.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of booleans.

;; The singleton truthy value, a function which ignores its second argument.
(def true (id const))

;; The singleton falsy value, a function which ignores its first argument.
(def false (const id))

;; True if both arguments are true.
(def and
  (fn x
    (fn y
      (if x (y ()) false))))

;; True if either argument is true.
(def or
  (fn x
    (fn y
      (if x true (y ())))))

;; True if its argument is false.
(def not
  (fn x
    (if x false true)))