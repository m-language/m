;;; Nat.m
;;;
;;; An implementation of natural numbers which are encoded as a function which
;;; applies a function to an argument N times.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of natural numbers.

;; The natural number 0.
(def nat.0 (fn f x x))

;; The natural number 1.
(def nat.1 (fn f x (f x)))

;; Tests if a natural number is 0.
(defn nat.0? n
  (n (const true) false))

;; The successor of a natural number.
(defn nat.inc n
  (fn f x
    (f (n f x))))

;; The predecessor of a natural number.
(defn nat.dec n
  (fn f x
    (n (fn g h (h (g f))) (const x) id)))

;; Adds two natural numbers.
(defnrec nat.+ a b
  (if (nat.0? b) a
    (nat.+ (nat.inc a) (nat.dec b))))

;; Subtracts two natural numbers.
(defnrec nat.- a b
  (if (nat.0? b) a
    (nat.- (nat.dec a) (nat.dec b))))

;; True if the first natural number is less than the second natural number.
(defnrec nat.< a b
  (cond
    (nat.0? a) (not (nat.0? b))
    (nat.0? b) false
    (nat.< (nat.dec a) (nat.dec b))))

;; True if the first natural number is greater than the second natural number.
(def nat.> (swap nat.<))

;; True if two natural numbers are equal.
(defnrec nat.= a b
  (cond
    (nat.0? a) (nat.0? b)
    (nat.0? b) false
    (nat.= (nat.dec a) (nat.dec b))))

;; Compares nats.
(defn compare-nat a b
  (cond
    (nat.> a b) compare/>
    (nat.< a b) compare/<
    compare/=))
