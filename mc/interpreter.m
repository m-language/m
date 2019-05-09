;; Interprets an operation.
(defn interpret-operation operation heap
  (interpret-operation' operation () heap))

;; Interprets an operation with a stack.
(defn interpret-operation' operation stack heap
  ((let type (type-name operation)
    (cond
      (symbol.= type (symbol local-variable-operation)) interpret-local-variable-operation
      (symbol.= type (symbol global-variable-operation)) interpret-global-variable-operation
      (symbol.= type (symbol if-operation)) interpret-if-operation
      (symbol.= type (symbol def-operation)) interpret-def-operation
      (symbol.= type (symbol fn-operation)) interpret-fn-operation
      (symbol.= type (symbol symbol-operation)) interpret-symbol-operation
      (symbol.= type (symbol apply-operation)) interpret-apply-operation
      (symbol.= type (symbol line-number-operation)) interpret-line-number-operation
      (symbol.= type (symbol nil-operation)) interpret-nil-operation
      (error (symbol "..."))))
    interpret-operation' operation stack heap))

;; Interprets a local variable operation.
(defn interpret-local-variable-operation interpret-operation' operation stack heap
  (get stack (local-variable-operation.index operation)))

;; Interprets a global variable operation.
(defn interpret-global-variable-operation interpret-operation' operation stack heap
  (heap.get heap (global-variable-operation.name operation)))

;; Interprets an if operation.
(defn interpret-if-operation interpret-operation' operation stack heap
  (if (interpret-operation' (if-operation.cond operation) stack heap)
    (interpret-operation' (if-operation.true operation) stack heap)
    (interpret-operation' (if-operation.false operation) stack heap)))

;; Interprets a def operation.
(defn interpret-def-operation interpret-operation' operation stack heap
  (heap.get heap (def-operation.name operation)))

;; Interprets a fn operation.
(defn interpret-fn-operation interpret-operation' operation stack heap
  (fn arg
    ((heap.get heap (fn-operation.name operation))
      (concat
        (map (fn-operation.closures operation)
          (fn closure
            (interpret-operation' closure stack heap)))
        (cons arg stack)))))

;; Interprets a symbol operation.
(defn interpret-symbol-operation interpret-operation' operation stack heap
  (symbol-operation.name operation))

;; Interperts an apply operation.
(defn interpret-apply-operation interpret-operation' operation stack heap
  ((interpret-operation' (apply-operation.fn operation) stack heap)
   (interpret-operation' (apply-operation.arg operation) stack heap)))

;; Interprets a line number operation.
(defn interpret-line-number-operation interpret-operation' operation
  (interpret-operation' (line-number-operation.operation operation)))

;; Interprets a nil operation.
(defn interpret-nil-operation interpret-operation' operation stack heap
  ())

;; Interprets a list of declarations.
(defn interpret-declarations declarations heap
  (fold declarations heap
    (fn heap' declaration
      (interpret-declaration declaration heap'))))

;; Interprets a declaration.
(defn interpret-declaration declaration heap
  ((let type (type-name declaration)
    (cond
      (symbol.= type (symbol def-declaration)) interpret-def-declaration
      (symbol.= type (symbol fn-declaration)) interpret-fn-declaration
      (error (symbol "..."))))
    declaration heap))

;; Interprets a def declaration.
(defn interpret-def-declaration declaration heap name
  (if (symbol.= name (def-declaration.name declaration))
    (some
      (fn heap'
        (interpret-operation (def-declaration.value declaration) heap')))
    (heap name)))

;; Interprets a fn declaration.
(defn interpret-fn-declaration declaration heap name
  (if (symbol.= name (fn-declaration.name declaration))
    (some
      (fn heap' stack
        (interpret-operation'
          (fn-declaration.value declaration)
          stack
          heap')))
    (heap name)))

;; The empty heap for the interpreter.
(def empty-heap (const null))

;; Gets a value in a heap.
(defn heap.get heap name
  (let value (heap name)
    (if (null? value)
      (error (concat (symbol->list (symbol "Could not find ")) name))
      ((unnull value) heap))))
