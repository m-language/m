;; A container for something.
(def some
  (new-data* (symbol some)
    (symbol value) ()))

;; Tests if a value is a some.
(def some? (is? (symbol some)))

(def some.value (field (symbol some) (symbol value)))

;; The lack of something.
(def none (object (symbol none)))

;; Tests if a value is none.
(def none? (is? (symbol none)))