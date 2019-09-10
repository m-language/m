;; Interprets a list of declarations.
(defn interpret-declarations declarations heap
  (fold declarations heap
    (fn heap' declaration
      (interpret-declaration declaration heap'))))

;; Interprets a declaration.
(extern interpret-declaration)

;; The empty heap for the interpreter.
(def empty-heap (const null))

;; Gets a value in a heap.
(defn heap.get heap name
  (let value (heap name)
    (if (null? value)
      (error (concat (symbol->list (symbol "Could not find ")) name))
      ((unnull value) heap))))
