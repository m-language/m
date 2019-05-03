;; An expression representing an M symbol.
(def symbol-expr
  (fn name path start end
    (pair true (cons name (cons path (cons start (cons end ())))))))

(def symbol-expr? first)
(def symbol-expr.name (compose car second))
(def symbol-expr.path (compose cadr second))
(def symbol-expr.start (compose caddr second))
(def symbol-expr.end (compose cadddr second))

;; An expression representing an M list.
(def list-expr
  (fn exprs path start end
    (pair false (cons exprs (cons path (cons start (cons end ())))))))

(def list-expr? (compose not first))
(def list-expr.exprs (compose car second))
(def list-expr.path (compose cadr second))
(def list-expr.start (compose caddr second))
(def list-expr.end (compose cadddr second))

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

;; Changes the path of an expr.
(defn expr.with-path path expr
  (if (symbol-expr? expr)
    (symbol-expr (symbol-expr.name expr) path (symbol-expr.start expr) (symbol-expr.end expr))
    (list-expr (map (list-expr.exprs expr) (expr.with-path path)) path (list-expr.start expr) (list-expr.end expr))))

(def expr/symbol
  (fn name
    (symbol-expr name (symbol expr.m) start-position start-position)))

(def expr/list
  (fn exprs
    (list-expr exprs (symbol expr.m) start-position start-position)))