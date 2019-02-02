;;; Nat.m
;;;
;;; An implementation of natural numbers which are encoded using lists of `()`
;;; whose length is equal to the number.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of natural numbers.

;; The natural number 0.
(def nat.0 ())

;; The natural number 1.
(def nat.1 (cons () ()))

;; Tests if a natural number is 0.
(def nat.0? nil?)

;; The successor of a natural number.
(def nat.inc (cons ()))

;; The predecessor of a natural number.
(def nat.dec (lambda x (cdr x)))

;; Adds two natural numbers.
(def nat.+
  (lambda x
    (lambda y
      (if (nat.0? y)
        x
        (nat.+ (nat.inc x) (nat.dec y))))))

;; Subtracts two natural numbers.
(def nat.-
  (lambda x
    (lambda y
      (if (nat.0? y)
        x
        (nat.- (nat.dec x) (nat.dec y))))))

;; True if the first natural number is less than the second natural number.
(def nat.<
  (lambda x
    (lambda y
      (if (nat.0? x)
        (not (nat.0? y))
        (if (nat.0? y)
          false
          (nat.< (nat.dec x) (nat.dec y)))))))

;; True if the first natural number is greater than the second natural number.
(def nat.> (swap nat.<))

;; True if two natural numbers are equal.
(def nat.=
  (lambda x
    (lambda y
      (if (nat.0? x)
        (nat.0? y)
        (if (nat.0? y)
          false
          (nat.= (nat.dec x) (nat.dec y)))))))