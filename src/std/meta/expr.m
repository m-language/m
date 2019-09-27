;; An expression representing an M symbol.
(def symbol-expr
  (fn name location
    (left (pair name location))))

;; An expression representing an M list.
(def list-expr
  (fn exprs location
    (right (pair exprs location))))

(defn expr.match expr symbol-fn list-fn
  (expr
    (fn symbol-expr (symbol-expr symbol-fn))
    (fn list-expr (list-expr list-fn))))

(defn expr.path expr
  (expr.match expr
    (fn _ l (location.path l))
    (fn _ l (location.path l))))

(defn expr.start expr
  (expr.match expr
    (fn _ l (span.from (location.span l)))
    (fn _ l (span.from (location.span l)))))

(defn expr.end expr
  (expr.match expr
    (fn _ l (span.to (location.span l)))
    (fn _ l (span.to (location.span l)))))

;; Changes the path of an expr.
(defnrec expr.with-path path expr
  (expr.match expr
    (fn name l (symbol-expr name (location path (location.span l))))
    (fn exprs l (list-expr (map exprs (expr.with-path path)) (location path (location.span l))))))

(def expr/symbol
  (fn name
    (symbol-expr name (location (symbol expr.m) (span start-position start-position)))))

(def expr/list
  (fn exprs
    (list-expr exprs (location (symbol expr.m) (span start-position start-position)))))

(defn expr/match expr symbol-fn list-fn
  (expr.match expr
    (fn symbol _ (symbol-fn symbol))
    (fn list _ (list-fn list))))

(def expr/nil
  (expr/list nil))

(defn expr/cons car cdr
  (expr/list
    (cons car
      (expr/match cdr
        (fn symbol (error symbol))
        (fn list list)))))
