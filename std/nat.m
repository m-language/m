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
(def nat.0?
  (fn n
    (n (const true) false)))

;; The successor of a natural number.
(def nat.inc
  (fn n
    (fn f x
      (f (n f x)))))

;; The predecessor of a natural number.
(def nat.dec
  (fn n
    (fn f x
      (n (fn g h (h (g f))) (const x) id))))

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

;; Compares nats.
(def compare-nat
  (fn a b
    (if (nat.> a b) compare>
    (if (nat.< a b) compare<
      compare=))))

;; Natural tests.
(def nat:test
  (apply-vararg combine-tests
    (assert     (nat.0? nat.0))
    (assert-not (nat.0? nat.1))
    (assert     (nat.0? (nat.dec nat.1)))
    (assert-not (nat.0? (nat.inc nat.0)))
    (test-compare compare-nat nat.0 nat.1)))