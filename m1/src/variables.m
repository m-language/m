;; The location of a local variable.
(def local-variable
  (new-data* (symbol local-variable)
    (symbol name) (symbol index) ()))

(def local-variable.name (field (symbol local-variable) (symbol name)))
(def local-variable.index (field (symbol local-variable) (symbol index)))

;; Tests if a value is a local variable.
(def local-variable? (is? (symbol local-variable)))

;; The location of a global variable
(def global-variable
  (new-data* (symbol global-variable)
    (symbol name) (symbol path) ()))

(def global-variable.name (field (symbol global-variable) (symbol name)))
(def global-variable.path (field (symbol global-variable) (symbol path)))

;; Tests if a value is a global variable.
(def global-variable? (is? (symbol global-variable)))