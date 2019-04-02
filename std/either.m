;;; Either.m
;;;
;;; An implementation of either as a higher order function whose argument is
;;; applied to the left or right value.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of either.

;; Creates a left either.
(def left
  (fn value
    (fn first ""
      (first value))))

;; Creates a right either.
(def right
  (fn value
    (fn "" second
      (second value))))

;; True if an either is left.
(def left?
  (fn either
    (either (const true) (const false))))

;; True if an either is right.
(def right?
  (fn either
    (either (const false) (const true))))