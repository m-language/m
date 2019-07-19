;;; Coproduct.m
;;;
;;; An implementation of coproducts as a function which applies a function to
;;; one of its arguments.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of coproducts.

;; Creates a coproduct with no alternatives.
(def coproduct/pure
  (fn value
    (fn f (f value))))

;; Extends a coproduct to the left.
(def coproduct/left
  (fn coproduct
    (fn _ right
      (coproduct right))))

;; Extends a coproduct to the right.
(def coproduct/right
  (fn coproduct
    (fn left _
      (coproduct left))))
