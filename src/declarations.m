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

(def def-declaration? (is? (symbol def-declaration)))

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

(def fn-declaration? (is? (symbol fn-declaration)))

;; The name of a declaration.
(def declaration.name
  (fn declaration
    (if (def-declaration? declaration)
      (def-declaration.name declaration)
      (fn-declaration.name declaration))))

;; The value of a declaration.
(def declaration.value
  (fn declaration
    (if (def-declaration? declaration)
      (def-declaration.value declaration)
      (fn-declaration.value declaration))))