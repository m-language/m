;;; Nat.m
;;;
;;; An implementation of natural numbers which are encoded using lists of `()`
;;; whose length is equal to the number.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of natural numbers.

;; The natural number 0.
(def nat.0 (left id))

;; The natural number 1.
(def nat.1 (right nat.0))

;; Tests if a natural number is 0.
(def nat.0? left?)

;; The successor of a natural number.
(def nat.inc right)

;; The predecessor of a natural number.
(def nat.dec
  (fn nat
    (nat left id)))

;; Adds two natural numbers.
(def nat.+
  (fn a b
    (if (nat.0? b) a
      (nat.+ (nat.inc a) (nat.dec b)))))

;; Subtracts two natural numbers.
(def nat.-
  (fn a b
    (if (nat.0? b) a
      (nat.- (nat.dec a) (nat.dec b)))))

;; True if the first natural number is less than the second natural number.
(def nat.<
  (fn a b
    (if (nat.0? a) (not (nat.0? b))
    (if (nat.0? b) false
      (nat.< (nat.dec a) (nat.dec b))))))

;; True if the first natural number is greater than the second natural number.
(def nat.> (swap nat.<))

;; True if two natural numbers are equal.
(def nat.=
  (fn a b
    (if (nat.0? a) (nat.0? b)
    (if (nat.0? b) false
      (nat.= (nat.dec a) (nat.dec b))))))