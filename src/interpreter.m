;; Interprets an operation.
(def interpret-operation
  (fn operation
    (fn heap
      (interpret-operation' operation () heap))))

;; Interprets an operation with a stack.
(def interpret-operation'
  (fn operation
    (fn stack
      (fn heap
        ((fn type
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
          (if (symbol.= type (symbol impure-operation))
            interpret-impure-operation
          (if (symbol.= type (symbol symbol-operation))
            interpret-symbol-operation
          (if (symbol.= type (symbol apply-operation))
            interpret-apply-operation
          (if (symbol.= type (symbol combine-operation))
            interpret-combine-operation
          (if (symbol.= type (symbol line-number-operation))
            interpret-line-number-operation
          (if (symbol.= type (symbol nil-operation))
            interpret-nil-operation
            (error (symbol "..."))))))))))))))
        (type-name operation)
          operation stack heap)))))

;; Interprets a local variable operation.
(def interpret-local-variable-operation
  (fn operation
    (fn stack
      (fn heap
        (get stack (local-variable-operation.index operation))))))

;; Interprets a global variable operation.
(def interpret-global-variable-operation
  (fn operation
    (fn stack
      (fn heap
        (with (heap (global-variable-operation.name operation))
        (fn value
          (if (null? value)
            (error
              (concat
                (symbol->list (symbol "Could not find value "))
                (global-variable-operation.name operation)))
            ((unnull value) heap))))))))

;; Interprets an if operation.
(def interpret-if-operation
  (fn operation
    (fn stack
      (fn heap
        (if (interpret-operation' (if-operation.cond operation) stack heap)
          (interpret-operation' (if-operation.true operation) stack heap)
          (interpret-operation' (if-operation.false operation) stack heap))))))

;; Interprets a def operation.
(def interpret-def-operation
  (fn operation
    (fn stack
      (fn heap
        (with (heap (def-operation.name operation))
        (fn value
          (if (null? value)
            (error
              (concat (symbol->list (symbol "Could not find value "))
                      (def-operation.name operation)))
            ((unnull value) heap))))))))

;; Interprets a fn operation.
(def interpret-fn-operation
  (fn operation
    (fn stack
      (fn heap
        (fn arg
          (with (heap (fn-operation.name operation))
          (fn function
            (if (null? function)
              (error (concat (symbol "Could not find synthetic function ")
                             (fn-operation.name operation)))
              ((unnull function)
                (concat (map (fn-operation.closures operation)
                    (fn closure (interpret-operation' closure stack heap)))
                  (cons arg stack))
                heap)))))))))

;; Interprets a impure operation.
(def interpret-impure-operation
  (fn operation
    (fn stack
      (fn heap
        (impure (interpret-operation' (impure-operation.operation operation) stack heap))))))

;; Interprets a symbol operation.
(def interpret-symbol-operation
  (fn operation
    (fn stack
      (fn heap
        (symbol-operation.name operation)))))

;; Interperts an apply operation.
(def interpret-apply-operation
  (fn operation
    (fn stack
      (fn heap
        ((interpret-operation' (apply-operation.fn operation) stack heap)
          (interpret-operation' (apply-operation.arg operation) stack heap))))))

;; Interpets a combine operation.
(def interpret-combine-operation
  (fn operation
    (fn stack
      (fn heap
        ((fn ignore (fn x x))
          (interpret-operation'
            (combine-operation.first operation)
            stack
            heap)
          (interpret-operation'
            (combine-operation.second operation)
            stack
            heap))))))

;; Interprets a line number operation.
(def interpret-line-number-operation
  (fn operation
    (interpret-operation' (line-number-operation.operation operation))))

;; Interprets a nil operation.
(def interpret-nil-operation
  (fn operation
    (fn stack
      (fn heap
        ()))))

;; Interprets a list of declarations
(def interpret-declarations
  (fn declarations
    (fn heap
      (fold declarations heap
        (fn heap'
          (fn declaration
            (interpret-declaration declaration heap')))))))

;; Interprets a declaration.
(def interpret-declaration
  (fn declaration
    (fn heap
      ((fn type
        (if (symbol.= type (symbol def-declaration))
          interpret-def-declaration
        (if (symbol.= type (symbol fn-declaration))
          interpret-fn-declaration
          (error (symbol "...")))))
      (type-name declaration)
        declaration heap))))

;; Interprets a def declaration.
(def interpret-def-declaration
  (fn declaration
    (fn heap
      (fn name
        (if (symbol.= name (def-declaration.name declaration))
          (some
            (fn heap'
              (interpret-operation
                (def-declaration.value declaration)
                heap')))
          (heap name))))))

;; Interprets a fn declaration.
(def interpret-fn-declaration
  (fn declaration
    (fn heap
      (fn name
        (if (symbol.= name (fn-declaration.name declaration))
          (some
            (fn stack
              (fn heap'
                (interpret-operation'
                  (fn-declaration.value declaration)
                  stack
                  heap'))))
          (heap name))))))

;; The empty heap for the interpreter.
(def empty-heap (const null))