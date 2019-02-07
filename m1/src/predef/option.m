;;; Option.m
;;;
;;; An implementation of nullable values encoded using a pair of a boolean and
;;; an optional value.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of null.

;; A container for a value.
(def some (ap pair true))

;; The singleton null value.
(def null (ap pair false ()))

;; Tests if an option has a value.
(def some? first)

;; Tests if an option is null.
(def null? (ap compose not first))

;; The value of an option, or `()` if it is null.
(def unnull second)