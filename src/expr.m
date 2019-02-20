;; An expression representing an M identifier.
(def identifier-expr
  (new-data (symbol identifier-expr)
    (list4 (symbol name) (symbol path) (symbol start) (symbol end))))

(def identifier-expr? (is? (symbol identifier-expr)))

(def identifier-expr.name (field (symbol identifier-expr) (symbol name)))
(def identifier-expr.path (field (symbol identifier-expr) (symbol path)))
(def identifier-expr.start (field (symbol identifier-expr) (symbol start)))
(def identifier-expr.end (field (symbol identifier-expr) (symbol end)))

;; An expression representing an M list.
(def list-expr
  (new-data (symbol list-expr)
    (list4 (symbol exprs) (symbol path) (symbol start) (symbol end))))

(def list-expr? (is? (symbol list-expr)))

(def list-expr.exprs (field (symbol list-expr) (symbol exprs)))
(def list-expr.path (field (symbol list-expr) (symbol path)))
(def list-expr.start (field (symbol list-expr) (symbol start)))
(def list-expr.end (field (symbol list-expr) (symbol end)))

(def expr.path
  (fn expr
    (if (list-expr? expr)
      (list-expr.path expr)
      (identifier-expr.path expr))))

(def expr.start
  (fn expr
    (if (list-expr? expr)
      (list-expr.start expr)
      (identifier-expr.start expr))))

(def expr.end
  (fn expr
    (if (list-expr? expr)
      (list-expr.end expr)
      (identifier-expr.end expr))))

;; Converts an expression to a list.
(def expr->list
  (fn expr
    (if (identifier-expr? expr)
      (left (identifier-expr.name expr))
      (right (map (list-expr.exprs expr) expr->list)))))

;; Converts a list to an expression.
(def list->expr
  (fn either
    (either
      (fn name
        (identifier-expr name () start-position start-position))
      (fn list
        (list-expr (map list list->expr) () start-position start-position)))))

;; A set of closures in an expression.
(def closures
  (fn expr
    (fn env'
      (if (identifier-expr? expr)
        ((fn variable
          (if (null? variable)
            (empty-tree-map compare-symbol)
            (if (local-variable? (unnull variable))
              (tree-map.put
                (empty-tree-map compare-symbol)
                (identifier-expr.name expr)
                true)
              (empty-tree-map compare-symbol))))
        (env.get env' (identifier-expr.name expr)))
        (fold (list-expr.exprs expr) (empty-tree-map compare-symbol)
          (fn map
            (fn expr
              (tree-map.+ map (closures expr env')))))))))