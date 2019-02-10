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

;; A function declaration.
(def fn-declaration
  (new-data (symbol fn-declaration)
    (list4 (symbol name) (symbol path) (symbol closures) (symbol value))))

(def fn-declaration.name
  (field (symbol fn-declaration) (symbol name)))

(def fn-declaration.path
  (field (symbol fn-declaration) (symbol path)))

(def fn-declaration.closures
  (field (symbol fn-declaration) (symbol closures)))

(def fn-declaration.value
  (field (symbol fn-declaration) (symbol value)))