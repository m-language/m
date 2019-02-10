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
  (fn compare
    (fn <
      (fn >
        (fn =
          (if (compare<? compare)
            (< compare)
            (if (compare>? compare)
              (> compare)
              (= compare))))))))

;; Compares two lists given a compare function.
(def compare-list
  (fn compare
    (fn list1
      (fn list2
        (if (and (nil? list1)
                 (fn "" (nil? list2)))
          compare=
        (if (nil? list1)
          compare<
        (if (nil? list2)
          compare>
        ((fn compare-result
          (if (compare=? compare-result)
            (compare-list compare (cdr list1) (cdr list2))
            compare-result))
          (compare (car list1) (car list2))))))))))

;; Compares nats.
(def compare-nat
  (fn nat1
    (fn nat2
      (if (nat.> nat1 nat2)
        compare>
        (if (nat.< nat1 nat2)
          compare<
          compare=)))))

;; Compares chars.
(def compare-char
  (fn char1
    (fn char2
      (compare-nat (char->nat char1) (char->nat char2)))))

;; Compares symbols.
(def compare-symbol (compare-list compare-char))