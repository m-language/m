;; A def declaration.
(def def-declaration
  (new-data (symbol def-declaration)
    (list3 (symbol name) (symbol path) (symbol value))))

;; A lambda declaration.
(def lambda-declaration
  (new-data (symbol lambda-declaration)
    (list4 (symbol name) (symbol path) (symbol closures) (symbol value))))