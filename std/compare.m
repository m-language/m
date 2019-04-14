;; Represents that two values are equal.
(def compare= (object (symbol compare=)))

;; Represents that the first value is less than the second value.
(def compare< (object (symbol compare<)))

;; Represents that the first value is greater than the second value.
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

;; Creates a test for a compare.
(def test-compare
  (fn compare min max
    (apply-vararg combine-tests
      (assert (compare<? (compare min max)))
      (assert (compare>? (compare max min)))
      (assert (compare=? (compare min min)))
      (assert (compare=? (compare max max))))))