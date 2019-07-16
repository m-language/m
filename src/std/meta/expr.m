;; An expression representing an M symbol.
(def symbol-expr
  (fn name path start end
    (fn f _
      (f
        (fn f
          (f name path start end))))))

;; An expression representing an M list.
(def list-expr
  (fn exprs path start end
    (fn _ f
      (f
        (fn f
          (f exprs path start end))))))

(defn expr.match expr symbol-fn list-fn
  (expr
    (fn symbol-expr (symbol-expr symbol-fn))
    (fn list-expr (list-expr list-fn))))

(defn expr.path expr
  (expr.match expr
    (fn _ path _ _ path)
    (fn _ path _ _ path)))

(defn expr.start expr
  (expr.match expr
    (fn _ _ start _ start)
    (fn _ _ start _ start)))

(defn expr.end expr
  (expr.match expr
    (fn _ _ _ end end)
    (fn _ _ _ end end)))

;; Changes the path of an expr.
(defnrec expr.with-path path expr
  (expr.match expr
    (fn name _ start end (symbol-expr name path start end))
    (fn exprs _ start end (list-expr (map exprs (expr.with-path path)) path start end))))

(def expr/symbol
  (fn name
    (symbol-expr name (symbol expr.m) start-position start-position)))

(def expr/list
  (fn exprs
    (list-expr exprs (symbol expr.m) start-position start-position)))
