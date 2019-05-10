;; The M backend for M.
(defn m-backend out operation declarations
  (let desugared (desugar-declarations declarations)
    (file.write out desugared)))

;; Desugars an operation.
(defn desugar-operation operation
  ((let type (type-name operation)
    (cond-satisfy (symbol.= type)
      (symbol local-variable-operation) desugar-local-variable-operation
      (symbol global-variable-operation) desugar-global-variable-operation
      (symbol def-operation) desugar-def-operation
      (symbol fn-operation) desugar-fn-operation
      (symbol symbol-operation) desugar-symbol-operation
      (symbol apply-operation) desugar-apply-operation
      (symbol line-number-operation) desugar-line-number-operation
      (symbol nil-operation) desugar-nil-operation
      (error (symbol "..."))))
    desugar-operation operation))

;; Desugars a local variable operation
(defn desugar-local-variable-operation desugar-operation operation
  (desugar-quote (local-variable-operation.name operation)))

;; Desugars a global variable operation
(defn desugar-global-variable-operation desugar-operation operation
  (desugar-quote (global-variable-operation.name operation)))

;; Desugars a def operation.
(defn desugar-def-operation desugar-operation operation
  (def-operation.name operation))

;; Desugars a fn operation.
(defn desugar-fn-operation desugar-operation operation
  (concat (symbol "(fn ")
  (concat (desugar-quote (fn-operation.arg operation))
  (concat (symbol " ")
  (concat (desugar-operation (fn-operation.value operation))
  (symbol ")"))))))

;; Desugars a symbol operation.
(defn desugar-symbol-operation desugar-operation operation
  (concat (symbol "(symbol ")
  (concat (desugar-quote (symbol-operation.name operation))
  (symbol ")"))))

;; Desugars an apply operation.
(defn desugar-apply-operation desugar-operation operation
  (concat (symbol "(")
  (concat (desugar-operation (apply-operation.fn operation))
  (concat (symbol " ")
  (concat (desugar-operation (apply-operation.arg operation))
  (symbol ")"))))))

;; Desugars a line number operation
(defn desugar-line-number-operation desugar-operation operation
  (desugar-operation (line-number-operation.operation operation)))

;; Desugars a nil operation.
(defn desugar-nil-operation desugar-operation operation
  (symbol "()"))

;; Desugars a list of declarations.
(defn desugar-declarations declarations
  (flat-map declarations desugar-declaration))

;; Desugars a declaration.
(defn desugar-declaration declaration
  ((let type (type-name declaration)
    (cond
      (symbol.= type (symbol def-declaration)) desugar-def-declaration
      (symbol.= type (symbol fn-declaration)) desugar-fn-declaration
      (error (symbol "..."))))
    declaration))

;; Desugars a def declaration.
(defn desugar-def-declaration declaration
  (concat (symbol "(def ")
  (concat (desugar-quote (def-declaration.name declaration))
  (concat (symbol " ")
  (concat (desugar-operation (def-declaration.value declaration))
  (append (symbol ")") linefeed))))))

;; Desugars a fn declaration.
(def desugar-fn-declaration (const ()))

;; Quotes a variable with invalid characters.
(defn desugar-quote name
  (if (| (desugar-should-quote? name) (nil? name))
    (cons quote
      (cons quote
        ((swap append) quote
          ((swap append) quote
            name))))
    name))

;; Tests if a name should be quoted
(defn desugar-should-quote? name
  (if (nil? name) false
    (let char (car name)
      (| (char.= char quote)
      (| (char.= char backslash)
      (| (char.= char open-parentheses)
      (| (char.= char close-parentheses)
      (| (char.= char semicolon)
      (| (whitespace? char)
         (desugar-should-quote? (cdr name)))))))))))
