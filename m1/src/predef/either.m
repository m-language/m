;;; Either.m
;;;
;;; An implementation of either as a higher order function whose argument is
;;; applied to the left or right value.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of either.

(def left
  (fn value
    (fn first
      (fn ""
        (ap first value)))))

(def right
  (fn value
    (fn ""
      (fn second
        (ap second value)))))

(def left?
  (fn either
    (ap either (ap const true) (ap const false))))

(def right?
  (fn either
    (ap either (ap const false) (ap const true))))