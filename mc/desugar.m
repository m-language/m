;; Desugars a file.
(def desugar-file
  (fn in out
    (then-run-with (generate in)
    (fn result
      (with (desugar-declarations (generated.declarations result))
      (fn desugared
        (file.write out desugared)))))))

;; Desugars an operation.
(def desugar-operation
  (fn operation
    ((fn type
      (if (symbol.= type (symbol local-variable-operation))
        desugar-local-variable-operation
      (if (symbol.= type (symbol global-variable-operation))
        desugar-global-variable-operation
      (if (symbol.= type (symbol def-operation))
        desugar-def-operation
      (if (symbol.= type (symbol fn-operation))
        desugar-fn-operation
      (if (symbol.= type (symbol symbol-operation))
        desugar-symbol-operation
      (if (symbol.= type (symbol apply-operation))
        desugar-apply-operation
      (if (symbol.= type (symbol line-number-operation))
        desugar-line-number-operation
      (if (symbol.= type (symbol nil-operation))
        desugar-nil-operation
        (error (symbol "...")))))))))))
    (type-name operation)
      desugar-operation operation)))

;; Desugars a local variable operation
(def desugar-local-variable-operation
  (fn desugar-operation operation
    (desugar-quote (local-variable-operation.name operation))))

;; Desugars a global variable operation
(def desugar-global-variable-operation
  (fn desugar-operation operation
    (desugar-quote (global-variable-operation.name operation))))

;; Desugars a def operation.
(def desugar-def-operation
  (fn desugar-operation operation
    (def-operation.name operation)))

;; Desugars a fn operation.
(def desugar-fn-operation
  (fn desugar-operation operation
    (concat (symbol "(fn ")
    (concat (desugar-quote (fn-operation.arg operation))
    (concat (symbol " ")
    (concat (desugar-operation (fn-operation.value operation))
    (symbol ")")))))))

;; Desugars a symbol operation.
(def desugar-symbol-operation
  (fn desugar-operation operation
    (concat (symbol "(symbol ")
    (concat (desugar-quote (symbol-operation.name operation))
    (symbol ")")))))

;; Desugars an apply operation.
(def desugar-apply-operation
  (fn desugar-operation operation
    (concat (symbol "(")
    (concat (desugar-operation (apply-operation.fn operation))
    (concat (symbol " ")
    (concat (desugar-operation (apply-operation.arg operation))
    (symbol ")")))))))

;; Desugars a line number operation
(def desugar-line-number-operation
  (fn desugar-operation operation
    (desugar-operation (line-number-operation.operation operation))))

;; Desugars a nil operation.
(def desugar-nil-operation
  (fn desugar-operation operation
    (symbol "()")))

;; Desugars a list of declarations.
(def desugar-declarations
  (fn declarations
    (flat-map declarations desugar-declaration)))

;; Desugars a declaration.
(def desugar-declaration
  (fn declaration
    ((fn type
      (if (symbol.= type (symbol def-declaration))
        desugar-def-declaration
      (if (symbol.= type (symbol fn-declaration))
        desugar-fn-declaration
        (error (symbol "...")))))
    (type-name declaration)
      declaration)))

;; Desugars a def declaration.
(def desugar-def-declaration
  (fn declaration
    (concat (symbol "(def ")
    (concat (desugar-quote (def-declaration.name declaration))
    (concat (symbol " ")
    (concat (desugar-operation (def-declaration.value declaration))
    (append (symbol ")") linefeed)))))))

;; Desugars a fn declaration.
(def desugar-fn-declaration (const ()))

;; Quotes a variable with invalid characters.
(def desugar-quote
  (fn name
    (if (| (desugar-should-quote? name) (nil? name))
      (cons quote
        (cons quote
          ((swap append) quote
            ((swap append) quote
              name))))
      name)))

;; Tests if a name should be quoted
(def desugar-should-quote?
  (fn name
    (if (nil? name) false
      (with (car name)
      (fn char
        (if (char.= char quote) true
        (if (char.= char backslash) true
        (if (char.= char open-parentheses) true
        (if (char.= char close-parentheses) true
        (if (char.= char semicolon) true
        (if (whitespace? char) true
        (desugar-should-quote? (cdr name)))))))))))))