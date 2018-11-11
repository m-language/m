(include data)
(include symbol)

;; Represents that two values are equal.
(def compare= (object (symbol compare=)))

;; Represents that the first value is greater than the second value.
(def compare< (object (symbol compare<)))

;; Represents that the first value is less than the second value.
(def compare> (object (symbol compare>)))

;; Tests if a value is compare=.
(def is-compare=
  (lambda x
    (eq-symbol (type-name x) (symbol compare=))))

;; Tests if a value is compare<.
(def is-compare<
  (lambda x
    (eq-symbol (type-name x) (symbol compare<))))

;; Tests if a value is compare>.
(def is-compare>
  (lambda x
    (eq-symbol (type-name x) (symbol compare>))))

;; Folds over the result of a compare.
(def fold-compare
  (lambda compare
    (lambda <
      (lambda >
        (lambda =
          (if (is-compare< compare)
            (< compare)
            (if (is-compare> compare)
              (> compare)
              (= compare))))))))

(include bool)
(include list)

;; Compares lists given [compare].
(def compare-list
  (lambda compare
    (lambda list1
      (lambda list2
        (if (and (is-nil list1)
                 (lambda unused (is-nil list2)))
          compare=
        (if (is-nil list1)
          compare<
        (if (is-nil list2)
          compare>
        ((lambda compare-result
          (if (is-compare= compare-result)
            (compare-list compare (cdr list1) (cdr list2))
            compare-result))
          (compare (car list1) (car list2))))))))))

(include int)

;; Compares ints.
(def compare-int
  (lambda int1
    (lambda int2
      (if (gt-int int1 int2)
        compare>
        (if (lt-int int1 int2)
          compare<
          compare=)))))

(include char)

;; Compares chars.
(def compare-char
  (lambda char1
    (lambda char2
      (compare-int (char->int char1) (char->int char2)))))

;; Compare strings.
(def compare-string (compare-list compare-char))