;; An expression representing an M symbol.
(def symbol-expr
  (fn name path start end
    (fn f _
      (f name path start end))))

;; An expression representing an M list.
(def list-expr
  (fn exprs path start end
    (fn _ f
      (f exprs path start end))))

(defn symbol-expr? expr
  (expr
    (fn _ _ _ _ true)
    (fn _ _ _ _ false)))

(defn list-expr? expr
  (expr
    (fn _ _ _ _ false)
    (fn _ _ _ _ true)))

(defn expr.path expr
  (expr
    (fn _ path _ _ path)
    (fn _ path _ _ path)))

(defn expr.start expr
  (expr
    (fn _ _ start _ start)
    (fn _ _ start _ start)))

(defn expr.end expr
  (expr
    (fn _ _ _ end end)
    (fn _ _ _ end end)))

;; Changes the path of an expr.
(defnrec expr.with-path path expr
  (expr
    (fn name _ start end (symbol-expr name path start end))
    (fn exprs _ start end (list-expr (map exprs (expr.with-path path)) path start end))))

(def expr/symbol
  (fn name
    (symbol-expr name (symbol expr.m) start-position start-position)))

(def expr/list
  (fn exprs
    (list-expr exprs (symbol expr.m) start-position start-position)))
