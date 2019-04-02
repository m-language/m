;; Represents that two values are equal.
(def compare= (object (symbol compare=)))

;; Represents that the first value is greater than the second value.
(def compare< (object (symbol compare<)))

;; Represents that the first value is less than the second value.
(def compare> (object (symbol compare>)))

;; Tests if a value is compare=.
(def compare=? (is? (symbol compare=)))

;; Tests if a value is compare<.
(def compare<? (is? (symbol compare<)))

;; Tests if a value is compare>.
(def compare>? (is? (symbol compare>)))

;; Folds over the result of a compare.
(def fold-compare
  (fn compare < > =
    (if (compare<? compare) (< compare)
    (if (compare>? compare) (> compare)
      (= compare)))))

;; Compares two lists given a compare function.
(def compare-list
  (fn compare a b
    (if (& (nil? a) (nil? b)) compare=
    (if (nil? a) compare<
    (if (nil? b) compare>
      (with (compare (car a) (car b))
      (fn compare-result
        (if (compare=? compare-result)
          (compare-list compare (cdr a) (cdr b))
          compare-result))))))))

;; Compares nats.
(def compare-nat
  (fn a b
    (if (nat.> a b) compare>
    (if (nat.< a b) compare<
      compare=))))

;; Compares chars.
(def compare-char
  (fn a b
    (compare-nat (char->nat a) (char->nat b))))

;; Compares symbols.
(def compare-symbol (compare-list compare-char))