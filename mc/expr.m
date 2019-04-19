;; An expression representing an M symbol.
(def symbol-expr
  (new-data (symbol symbol-expr)
    (list (symbol name) (symbol path) (symbol start) (symbol end))))

(def symbol-expr? (is? (symbol symbol-expr)))

(def symbol-expr.name (field (symbol symbol-expr) (symbol name)))
(def symbol-expr.path (field (symbol symbol-expr) (symbol path)))
(def symbol-expr.start (field (symbol symbol-expr) (symbol start)))
(def symbol-expr.end (field (symbol symbol-expr) (symbol end)))

;; An expression representing an M list.
(def list-expr
  (new-data (symbol list-expr)
    (list (symbol exprs) (symbol path) (symbol start) (symbol end))))

(def list-expr? (is? (symbol list-expr)))

(def list-expr.exprs (field (symbol list-expr) (symbol exprs)))
(def list-expr.path (field (symbol list-expr) (symbol path)))
(def list-expr.start (field (symbol list-expr) (symbol start)))
(def list-expr.end (field (symbol list-expr) (symbol end)))

(defn expr.path expr
  (if (list-expr? expr)
    (list-expr.path expr)
    (symbol-expr.path expr)))

(defn expr.start expr
  (if (list-expr? expr)
    (list-expr.start expr)
    (symbol-expr.start expr)))

(defn expr.end expr
 (if (list-expr? expr)
   (list-expr.end expr)
   (symbol-expr.end expr)))

;; Converts an expression to a list.
(defn expr->list expr
  (if (symbol-expr? expr)
    (left (symbol-expr.name expr))
    (right (map (list-expr.exprs expr) expr->list))))

;; Converts a list to an expression.
(defn list->expr expr either
  (either
    (fn name (symbol-expr name (expr.path expr) (expr.start expr) (expr.end expr)))
    (fn list (list-expr (map list (list->expr expr)) (expr.path expr) (expr.start expr) (expr.end expr)))))