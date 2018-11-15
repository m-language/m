;; A def declaration.
(def def-declaration
  (lambda name
    (lambda path
      (derive (symbol def-declaration) (symbol name) name
      (derive (symbol def-declaration) (symbol path) path
        (object (symbol def-declaration)))))))

;; A lambda declaration.
(def lambda-declaration
  (lambda name
    (lambda closures
      (lambda value
        (derive (symbol lambda-declaration) (symbol name) name
        (derive (symbol lambda-declaration) (symbol closures) closures
        (derive (symbol lambda-declaration) (symbol value) value
          (object (symbol lambda-declaration)))))))))

;; An import declaration
(def import-declaration
  (lambda name
    (derive (symbol import-declaration) (symbol name) name
      (object (symbol import-declaration)))))

;; Combines two declarations.
(def combine-declaration
  (lambda first
    (lambda second
      (derive (symbol combine-declaration) (symbol first) first
      (derive (symbol combine-declaration) (symbol second) second
        (object (symbol combine-declaration)))))))

;; Declaration which does nothing.
(def no-declaration (object (symbol no-declaration)))