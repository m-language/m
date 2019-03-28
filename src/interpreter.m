;; Interprets an operation.
(def interpret-operation
  (fn operation heap
    (interpret-operation' operation () heap)))

;; Interprets an operation with a stack.
(def interpret-operation'
  (fn operation stack heap
    ((with (type-name operation)
      (fn type
        (if (symbol.= type (symbol local-variable-operation))
          interpret-local-variable-operation
        (if (symbol.= type (symbol global-variable-operation))
          interpret-global-variable-operation
        (if (symbol.= type (symbol if-operation))
          interpret-if-operation
        (if (symbol.= type (symbol def-operation))
          interpret-def-operation
        (if (symbol.= type (symbol fn-operation))
          interpret-fn-operation
        (if (symbol.= type (symbol symbol-operation))
          interpret-symbol-operation
        (if (symbol.= type (symbol apply-operation))
          interpret-apply-operation
        (if (symbol.= type (symbol line-number-operation))
          interpret-line-number-operation
        (if (symbol.= type (symbol nil-operation))
          interpret-nil-operation
          (error (symbol "...")))))))))))))
      interpret-operation' operation stack heap)))

;; Interprets a local variable operation.
(def interpret-local-variable-operation
  (fn interpret-operation' operation stack heap
    (get stack (local-variable-operation.index operation))))

;; Interprets a global variable operation.
(def interpret-global-variable-operation
  (fn interpret-operation' operation stack heap
    (heap.get heap (global-variable-operation.name operation))))

;; Interprets an if operation.
(def interpret-if-operation
  (fn interpret-operation' operation stack heap
    (if (interpret-operation' (if-operation.cond operation) stack heap)
      (interpret-operation' (if-operation.true operation) stack heap)
      (interpret-operation' (if-operation.false operation) stack heap))))

;; Interprets a def operation.
(def interpret-def-operation
  (fn interpret-operation' operation stack heap
    (heap.get heap (def-operation.name operation))))

;; Interprets a fn operation.
(def interpret-fn-operation
  (fn interpret-operation' operation stack heap
    (fn arg
      ((heap.get heap (fn-operation.name operation))
        (concat
          (map (fn-operation.closures operation)
            (fn closure (interpret-operation' closure stack heap))))
          (cons arg stack)))))

;; Interprets a symbol operation.
(def interpret-symbol-operation
  (fn interpret-operation' operation stack heap
    (symbol-operation.name operation)))

;; Interperts an apply operation.
(def interpret-apply-operation
  (fn interpret-operation' operation stack heap
    ((interpret-operation' (apply-operation.fn operation) stack heap)
      (interpret-operation' (apply-operation.arg operation) stack heap))))

;; Interprets a line number operation.
(def interpret-line-number-operation
  (fn interpret-operation' operation
    (interpret-operation' (line-number-operation.operation operation))))

;; Interprets a nil operation.
(def interpret-nil-operation
  (fn interpret-operation' operation stack heap
    ()))

;; Interprets a list of declarations.
(def interpret-declarations
  (fn declarations heap
    (fold declarations heap
      (fn heap' declaration
        (interpret-declaration declaration heap')))))

;; Interprets a declaration.
(def interpret-declaration
  (fn declaration heap
    ((with (type-name declaration)
      (fn type
        (if (symbol.= type (symbol def-declaration))
          interpret-def-declaration
        (if (symbol.= type (symbol fn-declaration))
          interpret-fn-declaration
          (error (symbol "..."))))))
    (type-name declaration)
      declaration heap)))

;; Interprets a def declaration.
(def interpret-def-declaration
  (fn declaration heap name
    (if (symbol.= name (def-declaration.name declaration))
      (some
        (fn heap'
          (interpret-operation (def-declaration.value declaration) heap')))
      (heap name))))

;; Interprets a fn declaration.
(def interpret-fn-declaration
  (fn declaration heap name
    (if (symbol.= name (fn-declaration.name declaration))
      (some
        (fn stack heap'
          (interpret-operation'
            (fn-declaration.value declaration)
            stack
            heap')))
      (heap name))))

;; The empty heap for the interpreter.
(def empty-heap (const null))

;; Adds two heaps.
(def heap.+
  (fn heap1 heap2 name
    (with (heap1 name)
    (fn value
      (if (some? value) value (heap2 name))))))

;; Gets a value in a heap.
(def heap.get
  (fn heap name
    (with (heap name)
    (fn value
      (if (null? value)
        (error (concat (symbol->list (symbol "Could not find ")) name))
        ((unnull value) heap))))))