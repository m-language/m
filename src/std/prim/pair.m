;;; Pair.m
;;;
;;; An implementation of pairs as a function from a boolean to a value, where
;;; `(pair true)` is the first value of the pair and `(pair false)` is the
;;; second value of the pair.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of pairs.

;; Creates a pair of two values.
(def pair
  (fn first second
    (fn f
      (f first second))))

;; The first value of a pair.
(def first
  (fn pair
    (pair true)))

;; The second value of a pair.
(def second
  (fn pair
    (pair false)))