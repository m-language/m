;; Interprets an operation.
(def interpret-operation
  (lambda operation
    (lambda stack
      (lambda heap
        ((lambda type
          (if (symbol.= type (symbol local-variable-operation))
            interpret-local-variable-operation
          (if (symbol.= type (symbol global-variable-operation))
            interpret-global-variable-operation
          (if (symbol.= type (symbol if-operation))
            interpret-if-operation
          (if (symbol.= type (symbol def-operation))
            interpret-def-operation
          (if (symbol.= type (symbol lambda-operation))
            interpret-lambda-operation
          (if (symbol.= type (symbol do-operation))
            interpret-do-operation
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
  (lambda operation
    (lambda stack
      (lambda heap
        (get stack (local-variable-operation.index operation))))))

;; Interprets a global variable operation.
(def interpret-global-variable-operation
  (lambda operation
    (lambda stack
      (lambda heap
        (with (tree-map.get heap (global-variable-operation.name operation))
        (lambda value
          (if (null? value)
            (error (concat (symbol->list (symbol "Could not find value "))
                           (global-variable-operation.name operation)))
            (unnull value))))))))

;; Interprets an if operation.
(def interpret-if-operation
  (lambda operation
    (lambda stack
      (lambda heap
        (if (interpret-operation (if-operation.cond operation) stack heap)
          (interpret-operation (if-operation.true operation) stack heap)
          (interpret-operation (if-operation.false operation) stack heap))))))

;; Interprets a def operation.
(def interpret-def-operation
  (lambda operation
    (lambda stack
      (lambda heap
        (with (tree-map.get heap (def-operation.name operation))
        (lambda value
          (if (null? value)
            (error (concat (symbol->list (symbol "Could not find value "))
                   (def-operation.name operation)))
            (unnull value))))))))

;; Interprets a lambda operation.
(def interpret-lambda-operation
  (lambda operation
    (lambda stack
      (lambda heap
        (lambda arg
          (with (tree-map.get heap (lambda-operation.name operation))
          (lambda function
            (if (null? function)
              (error (concat (symbol->list (symbol "Could not find synthetic function "))
                             (lambda-operation.name operation)))
              ((unnull function)
                (concat (map (lambda-operation.closures operation)
                    (lambda closure (interpret-operation closure stack heap)))
                  (cons arg stack))
                heap)))))))))

;; Interprets a do operation.
(def interpret-do-operation
  (lambda operation
    (lambda stack
      (lambda heap
        (do (interpret-operation (do-operation operation) stack heap))))))

;; Interprets a symbol operation.
(def interpret-symbol-operation
  (lambda operation
    (lambda stack
      (lambda heap
        (symbol-operation.name operation)))))

;; Interperts an apply operation.
(def interpret-apply-operation
  (lambda operation
    (lambda stack
      (lambda heap
        ((interpret-operation (apply-operation.fn operation) stack heap)
          (interpret-operation (apply-operation.arg operation) stack heap))))))

;; Interpets a combine operation.
(def interpret-combine-operation
  (lambda operation
    (lambda stack
      (lambda heap
        ((lambda ignore (lambda x x))
          (interpret-operation
            (combine-operation.first operation)
            stack
            heap)
          (interpret-operation
            (combine-operation.second operation)
            stack
            heap))))))

;; Interprets a line number operation.
(def interpret-line-number-operation
  (lambda operation
    (interpret-operation (line-number-operation.operation operation))))

;; Interprets a nil operation.
(def interpret-nil-operation
  (lambda operation
    (lambda stack
      (lambda heap
        ()))))

;; Interprets a list of declarations
(def interpret-declarations
  (lambda declarations
    (lambda heap
      (fold declarations heap
        (lambda heap'
          (lambda declaration
            (interpret-declaration declaration heap')))))))

;; Interprets a declaration.
(def interpret-declaration
  (lambda declaration
    (lambda heap
      ((lambda type
        (if (symbol.= type (symbol def-declaration))
          interpret-def-declaration
        (if (symbol.= type (symbol lambda-declaration))
          interpret-lambda-declaration
          (error (symbol "...")))))
      (type-name declaration)
        declaration heap))))

;; Interprets a def declaration.
(def interpret-def-declaration
  (lambda declaration
    (lambda heap
      (tree-map.put heap
        (def-declaration.name declaration)
        (interpret-operation
          (def-declaration.value declaration)
          ()
          heap)))))

;; Interprets a lambda declaration.
(def interpret-lambda-declaration
  (lambda declaration
    (lambda heap
      (tree-map.put heap
        (lambda-declaration.name declaration)
        (lambda stack
          (lambda heap'
            (interpret-operation
              (lambda-declaration.value declaration)
              stack
              heap')))))))

;; The default heap for the interpreter.
(def default-heap (empty-tree-map compare-symbol))