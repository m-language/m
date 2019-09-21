;; The location of a local variable.
(def local-variable
  (new-data (symbol local-variable)
    (list (symbol name))))

(def local-variable.name (field (symbol local-variable) (symbol name)))
(def local-variable? (is? (symbol local-variable)))

;; The location of a global variable
(def global-variable
  (new-data (symbol global-variable)
    (list (symbol name) (symbol macro?))))

(def global-variable.name (field (symbol global-variable) (symbol name)))
(def global-variable.macro? (field (symbol global-variable) (symbol macro?)))
(def global-variable? (is? (symbol global-variable)))
