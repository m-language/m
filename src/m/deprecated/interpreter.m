;; Interprets a list of declarations.
(defn interpret-declarations declarations heap
  (fold declarations heap
    (fn heap' declaration
      (interpret-declaration declaration heap'))))

;; Interprets a declaration.
(extern interpret-declaration)
