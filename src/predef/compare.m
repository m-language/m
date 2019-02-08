;; Represents that two values are equal.
(def compare= (ap object (symbol compare=)))

;; Represents that the first value is greater than the second value.
(def compare< (ap object (symbol compare<)))

;; Represents that the first value is less than the second value.
(def compare> (ap object (symbol compare>)))

;; Tests if a value is compare=.
(def compare=? (ap is? (symbol compare=)))

;; Tests if a value is compare<.
(def compare<? (ap is? (symbol compare<)))

;; Tests if a value is compare>.
(def compare>? (ap is? (symbol compare>)))

;; Folds over the result of a compare.
(def fold-compare
  (fn compare
    (fn <
      (fn >
        (fn =
          (if (ap compare<? compare)
            (ap < compare)
            (if (ap compare>? compare)
              (ap > compare)
              (ap = compare))))))))

;; Compares two lists given a compare function.
(def compare-list
  (fn compare
    (fn list1
      (fn list2
        (if (ap and (ap nil? list1)
                 (fn "" (ap nil? list2)))
          compare=
        (if (ap nil? list1)
          compare<
        (if (ap nil? list2)
          compare>
        (ap (fn compare-result
          (if (ap compare=? compare-result)
            (ap compare-list compare (ap cdr list1) (ap cdr list2))
            compare-result))
          (ap compare (ap car list1) (ap car list2))))))))))

;; Compares nats.
(def compare-nat
  (fn nat1
    (fn nat2
      (if (ap nat.> nat1 nat2)
        compare>
        (if (ap nat.< nat1 nat2)
          compare<
          compare=)))))

;; Compares chars.
(def compare-char
  (fn char1
    (fn char2
      (ap compare-nat (ap char->nat char1) (ap char->nat char2)))))

;; Compares symbols.
(def compare-symbol (ap compare-list compare-char))