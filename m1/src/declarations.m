;; A def declaration.
(def def-declaration
  (ap new-data (symbol def-declaration)
    (ap list3 (symbol name) (symbol path) (symbol value))))

(def def-declaration.name
  (ap field (symbol def-declaration) (symbol name)))

(def def-declaration.path
  (ap field (symbol def-declaration) (symbol path)))

(def def-declaration.value
  (ap field (symbol def-declaration) (symbol value)))

;; A function declaration.
(def fn-declaration
  (ap new-data (symbol fn-declaration)
    (ap list4 (symbol name) (symbol path) (symbol closures) (symbol value))))

(def fn-declaration.name
  (ap field (symbol fn-declaration) (symbol name)))

(def fn-declaration.path
  (ap field (symbol fn-declaration) (symbol path)))

(def fn-declaration.closures
  (ap field (symbol fn-declaration) (symbol closures)))

(def fn-declaration.value
  (ap field (symbol fn-declaration) (symbol value)))