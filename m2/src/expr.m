(import predef)

;; An expression representing an M identifier.
(def identifier-expr
  (lambda name
    (lambda line
      (derive (symbol identifier-expr) (symbol name) name
      (derive (symbol identifier-expr) (symbol line) line
        (object (symbol identifier-expr)))))))

;; True if [x] is an identifier expression.
(def is-identifier-expr
  (lambda x
    (eq-symbol (type-name x) (symbol identifier-expr))))

(def identifier-expr.name (field (symbol identifier-expr) (symbol name)))
(def identifier-expr.line (field (symbol identifier-expr) (symbol line)))

;; An expression representing an M list.
(def list-expr
  (lambda exprs
    (lambda line
      (derive (symbol list-expr) (symbol exprs) exprs
      (derive (symbol list-expr) (symbol line) line
        (object (symbol list-expr)))))))

;; True if [x] is a list expression.
(def is-list-expr
  (lambda x
    (eq-symbol (type-name x) (symbol list-expr))))

(def list-expr.exprs (field (symbol list-expr) (symbol exprs)))
(def list-expr.line (field (symbol list-expr) (symbol line)))

;; The line of an expression.
(def expr.line
  (lambda expr
    (if (is-identifier-expr expr)
      (identifier-expr.line expr)
      (list-expr.line expr))))