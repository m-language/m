;; A def declaration.
(def def-declaration
  (new-data (symbol def-declaration)
    (list3 (symbol name) (symbol path) (symbol value))))

(def def-declaration.name
  (field (symbol def-declaration) (symbol name)))

(def def-declaration.path
  (field (symbol def-declaration) (symbol path)))

(def def-declaration.value
  (field (symbol def-declaration) (symbol value)))

;; A lambda declaration.
(def lambda-declaration
  (new-data (symbol lambda-declaration)
    (list4 (symbol name) (symbol path) (symbol closures) (symbol value))))

(def lambda-declaration.name
  (field (symbol lambda-declaration) (symbol name)))

(def lambda-declaration.path
  (field (symbol lambda-declaration) (symbol path)))

(def lambda-declaration.closures
  (field (symbol lambda-declaration) (symbol closures)))

(def lambda-declaration.value
  (field (symbol lambda-declaration) (symbol value)))