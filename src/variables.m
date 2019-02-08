;; The location of a local variable.
(def local-variable
  (ap new-data (symbol local-variable)
    (ap list2 (symbol name) (symbol index))))

(def local-variable.name (ap field (symbol local-variable) (symbol name)))
(def local-variable.index (ap field (symbol local-variable) (symbol index)))

;; Tests if a value is a local variable.
(def local-variable? (ap is? (symbol local-variable)))

;; The location of a global variable
(def global-variable
  (ap new-data (symbol global-variable)
    (ap list2 (symbol name) (symbol path))))

(def global-variable.name (ap field (symbol global-variable) (symbol name)))
(def global-variable.path (ap field (symbol global-variable) (symbol path)))

;; Tests if a value is a global variable.
(def global-variable? (ap is? (symbol global-variable)))