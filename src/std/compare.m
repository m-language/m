;; A data type representing the result of a comparison.
(defdata compare
  ;; Represents that the first value is less than the second value.
  <
  ;; Represents that the first value is greater than the second value.
  >
  ;; Represents that two values are equal.
  =)

(defn compare/<? compare (compare.match compare true false false))
(defn compare/>? compare (compare.match compare false true false))
(defn compare/=? compare (compare.match compare false false true))

(defn fold-compare compare < > =
  (compare
    (fn compare/< (compare/< (< compare/<)))
    (fn compare/> (compare/> (> compare/>)))
    (fn compare/= (compare/= (= compare/=)))))
