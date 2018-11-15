(import predef.data)
(import predef.symbol)

;; A container for something.
(def some
  (lambda value
    (derive (symbol some) (symbol value) value
      (object (symbol some)))))

;; Tests if a value is a some.
(def is-some
  (lambda x
    (eq-symbol (type-name x) (symbol some))))

(def some.value (field (symbol some) (symbol value)))

;; The lack of something.
(def none (object (symbol none)))

;; Tests if a value is none.
(def is-none
  (lambda x
    (eq-symbol (type-name x) (symbol none))))