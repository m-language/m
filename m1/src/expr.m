;; An expression representing an M identifier.
(def identifier-expr
  (ap new-data (symbol identifier-expr)
    (ap list4 (symbol name) (symbol path) (symbol start) (symbol end))))

(def identifier-expr? (ap is? (symbol identifier-expr)))

(def identifier-expr.name (ap field (symbol identifier-expr) (symbol name)))
(def identifier-expr.path (ap field (symbol identifier-expr) (symbol path)))
(def identifier-expr.start (ap field (symbol identifier-expr) (symbol start)))
(def identifier-expr.end (ap field (symbol identifier-expr) (symbol end)))

;; An expression representing an M list.
(def list-expr
  (ap new-data (symbol list-expr)
    (ap list4 (symbol exprs) (symbol path) (symbol start) (symbol end))))

(def list-expr? (ap is? (symbol list-expr)))

(def list-expr.exprs (ap field (symbol list-expr) (symbol exprs)))
(def list-expr.path (ap field (symbol list-expr) (symbol path)))
(def list-expr.start (ap field (symbol list-expr) (symbol start)))
(def list-expr.end (ap field (symbol list-expr) (symbol end)))

(def expr.path
  (fn expr
    (if (ap list-expr? expr)
      (ap list-expr.path expr)
      (ap identifier-expr.path expr))))

(def expr.start
  (fn expr
    (if (ap list-expr? expr)
      (ap list-expr.start expr)
      (ap identifier-expr.start expr))))

(def expr.end
  (fn expr
    (if (ap list-expr? expr)
      (ap list-expr.end expr)
      (ap identifier-expr.end expr))))

;; Converts an expression to a list.
(def expr->list
  (fn expr
    (if (ap identifier-expr? expr)
      (ap left (ap identifier-expr.name expr))
      (ap right (ap map (ap list-expr.exprs expr) expr->list)))))

;; Converts a list to an expression.
(def list->expr
  (fn either
    (ap either
      (fn name
        (ap identifier-expr name () start-position start-position))
      (fn list
        (ap list-expr (ap map list list->expr) () start-position start-position)))))