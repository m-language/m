(import data)
(import symbol)

;; A pair of two values.
(def pair
  (lambda first
    (lambda second
      (derive (symbol pair) (symbol first) first
      (derive (symbol pair) (symbol second) second
        (object (symbol pair)))))))

;; The first value in a pair.
(def first (field (symbol pair) (symbol first)))

;; The second value in a pair.
(def second (field (symbol pair) (symbol second)))