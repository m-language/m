;; Interprets an operation.
(def interpret-operation
  (fn operation
    (fn heap
      (ap interpret-operation' operation () heap))))

;; Interprets an operation with a stack.
(def interpret-operation'
  (fn operation
    (fn stack
      (fn heap
        (ap (fn type
          (if (ap symbol.= type (symbol local-variable-operation))
            interpret-local-variable-operation
          (if (ap symbol.= type (symbol global-variable-operation))
            interpret-global-variable-operation
          (if (ap symbol.= type (symbol if-operation))
            interpret-if-operation
          (if (ap symbol.= type (symbol def-operation))
            interpret-def-operation
          (if (ap symbol.= type (symbol fn-operation))
            interpret-fn-operation
          (if (ap symbol.= type (symbol impure-operation))
            interpret-impure-operation
          (if (ap symbol.= type (symbol symbol-operation))
            interpret-symbol-operation
          (if (ap symbol.= type (symbol apply-operation))
            interpret-apply-operation
          (if (ap symbol.= type (symbol combine-operation))
            interpret-combine-operation
          (if (ap symbol.= type (symbol line-number-operation))
            interpret-line-number-operation
          (if (ap symbol.= type (symbol nil-operation))
            interpret-nil-operation
            (ap error (symbol "..."))))))))))))))
        (ap type-name operation)
          operation stack heap)))))

;; Interprets a local variable operation.
(def interpret-local-variable-operation
  (fn operation
    (fn stack
      (fn heap
        (ap get stack (ap local-variable-operation.index operation))))))

;; Interprets a global variable operation.
(def interpret-global-variable-operation
  (fn operation
    (fn stack
      (fn heap
        (ap with
          (ap heap (ap global-variable-operation.name operation))
        (fn value
          (if (ap null? value)
            (ap error
              (ap concat
                (ap symbol->list (symbol "Could not find value "))
                (ap global-variable-operation.name operation)))
            (ap (ap unnull value) heap))))))))

;; Interprets an if operation.
(def interpret-if-operation
  (fn operation
    (fn stack
      (fn heap
        (if (ap interpret-operation' (ap if-operation.cond operation) stack heap)
          (ap interpret-operation' (ap if-operation.true operation) stack heap)
          (ap interpret-operation' (ap if-operation.false operation) stack heap))))))

;; Interprets a def operation.
(def interpret-def-operation
  (fn operation
    (fn stack
      (fn heap
        (ap with (ap heap (ap def-operation.name operation))
        (fn value
          (if (ap null? value)
            (ap error
              (ap concat (ap symbol->list (symbol "Could not find value \""))
              (ap concat (ap def-operation.name operation)
                (symbol "\""))))
            (ap unnull value))))))))

;; Interprets a fn operation.
(def interpret-fn-operation
  (fn operation
    (fn stack
      (fn heap
        (fn arg
          (ap with (ap heap (ap fn-operation.name operation))
          (fn function
            (if (ap null? function)
              (ap error (ap concat (symbol "Could not find synthetic function ")
                        (ap fn-operation.name operation)))
              (ap (ap unnull function)
                (ap concat (ap map (ap fn-operation.closures operation)
                    (fn closure (ap interpret-operation' closure stack heap)))
                  (ap cons arg stack))
                heap)))))))))

;; Interprets a impure operation.
(def interpret-impure-operation
  (fn operation
    (fn stack
      (fn heap
        (impure (ap interpret-operation' (ap impure-operation operation) stack heap))))))

;; Interprets a symbol operation.
(def interpret-symbol-operation
  (fn operation
    (fn stack
      (fn heap
        (ap symbol-operation.name operation)))))

;; Interperts an apply operation.
(def interpret-apply-operation
  (fn operation
    (fn stack
      (fn heap
        (ap (ap interpret-operation' (ap apply-operation.fn operation) stack heap)
          (ap interpret-operation' (ap apply-operation.arg operation) stack heap))))))

;; Interpets a combine operation.
(def interpret-combine-operation
  (fn operation
    (fn stack
      (fn heap
        (ap (fn ignore (fn x x))
          (ap interpret-operation'
            (ap combine-operation.first operation)
            stack
            heap)
          (ap interpret-operation'
            (ap combine-operation.second operation)
            stack
            heap))))))

;; Interprets a line number operation.
(def interpret-line-number-operation
  (fn operation
    (ap interpret-operation' (ap line-number-operation.operation operation))))

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
      (ap fold declarations heap
        (fn heap'
          (fn declaration
            (ap interpret-declaration declaration heap')))))))

;; Interprets a declaration.
(def interpret-declaration
  (fn declaration
    (fn heap
      (ap (fn type
        (if (ap symbol.= type (symbol def-declaration))
          interpret-def-declaration
        (if (ap symbol.= type (symbol fn-declaration))
          interpret-fn-declaration
          (ap error (symbol "...")))))
      (ap type-name declaration)
        declaration heap))))

;; Interprets a def declaration.
(def interpret-def-declaration
  (fn declaration
    (fn heap
      (fn name
        (if (ap symbol.= name (ap def-declaration.name declaration))
          (fn heap'
            (ap interpret-operation'
              (ap def-declaration.value declaration)
              ()
              heap'))
          (ap heap name))))))

;; Interprets a fn declaration.
(def interpret-fn-declaration
  (fn declaration
    (fn heap
      (fn name
        (if (ap symbol.= name (ap fn-declaration.name declaration))
          (fn stack
            (fn heap'
              (ap interpret-operation'
                (ap fn-declaration.value declaration)
                stack
                heap')))
          (ap heap name))))))

;; The empty heap for the interpreter.
(def empty-heap (ap const null))