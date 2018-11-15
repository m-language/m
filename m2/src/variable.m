(import predef)

;; The location of a local variable.
(def local-variable
  (lambda name
    (lambda index
      (derive (symbol local-variable) (symbol name) name
      (derive (symbol local-variable) (symbol index) index
        (object (symbol local-variable)))))))

(def local-variable.name (field (symbol local-variable) (symbol name)))
(def local-variable.index (field (symbol local-variable) (symbol index)))

;; Tests if a value is a local variable.
(def is-local-variable
  (lambda x
    (eq-symbol (type-name x) (symbol local-variable))))

;; The location of a global variable
(def global-variable
  (lambda name
    (lambda path
      (derive (symbol global-variable) (symbol name) name
      (derive (symbol global-variable) (symbol path) path
        (object (symbol global-variable)))))))

(def global-variable.name (field (symbol global-variable) (symbol name)))
(def global-variable.path (field (symbol global-variable) (symbol path)))

;; Tests if a value is a global variable.
(def is-global-variable
  (lambda x
    (eq-symbol (type-name x) (symbol global-variable))))