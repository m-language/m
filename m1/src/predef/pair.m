;;; Pair.m
;;;
;;; An implementation of pairs as a function from a boolean to a value, where
;;; `(pair true)` is the left value of the pair and `(pair false)` is the right
;;; value of the pair.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of pairs.

;; Creates a pair of two values.
(def pair
  (lambda left
    (lambda right
      (lambda x
        (x left right)))))

;; The left value of a pair.
(def left
  (lambda pair
    (pair true)))

;; The right value of a pair.
(def right
  (lambda pair
    (pair false)))