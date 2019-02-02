;;; Either.m
;;;
;;; An implementation of either as a higher order function whose argument is
;;; applied to the left or right value.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of either.

(def left
  (lambda value
    (lambda first
      (lambda ""
        (first value)))))

(def right
  (lambda value
    (lambda ""
      (lambda second
        (second value)))))

(def left?
  (lambda either
    (either (const true) (const false))))

(def right?
  (lambda either
    (either (const false) (const true))))