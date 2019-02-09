;;; Nat.m
;;;
;;; An implementation of natural numbers which are encoded using lists of `()`
;;; whose length is equal to the number.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of natural numbers.

;; The natural number 0.
(def nat.0 (ap left id))

;; The natural number 1.
(def nat.1 (ap right nat.0))

;; Tests if a natural number is 0.
(def nat.0? left?)

;; The successor of a natural number.
(def nat.inc right)

;; The predecessor of a natural number.
(def nat.dec
  (fn nat
    (ap nat left id)))

;; Adds two natural numbers.
(def nat.+
  (fn x
    (fn y
      (if (ap nat.0? y)
        x
        (ap nat.+ (ap nat.inc x) (ap nat.dec y))))))

;; Subtracts two natural numbers.
(def nat.-
  (fn x
    (fn y
      (if (ap nat.0? y)
        x
        (ap nat.- (ap nat.dec x) (ap nat.dec y))))))

;; True if the first natural number is less than the second natural number.
(def nat.<
  (fn x
    (fn y
      (if (ap nat.0? x)
        (ap not (ap nat.0? y))
        (if (ap nat.0? y)
          false
          (ap nat.< (ap nat.dec x) (ap nat.dec y)))))))

;; True if the first natural number is greater than the second natural number.
(def nat.> (ap swap nat.<))

;; True if two natural numbers are equal.
(def nat.=
  (fn x
    (fn y
      (if (ap nat.0? x)
        (ap nat.0? y)
        (if (ap nat.0? y)
          false
          (ap nat.= (ap nat.dec x) (ap nat.dec y)))))))