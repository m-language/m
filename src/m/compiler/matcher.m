;; Matches a function expression.
(defn match-fn-expr exprs no-expr no-args invalid-arg success
  (if (nil? exprs) no-expr
  (if (nil? (cdr exprs)) no-args
    (match-fn-expr' exprs nil invalid-arg success)
    (success (map (init (cdr exprs)) symbol-expr.name) (last exprs)))))

(defn match-fn-expr' exprs names invalid-arg success
  (if (nil? (cdr exprs))
    (success names (car exprs))
    ((car exprs)
      (fn name _ _ _
        (match-fn-expr' (cdr exprs) (append name names) invalid-arg success))
      (fn _ _ _ _
        (invalid-arg (car exprs))))))

(defn match-def-expr exprs no-name no-expr invalid-name success
  (if (nil? exprs) no-name
  (if (nil? (cdr exprs)) no-expr
    ((car exprs)
      (fn name _ _ _ (success name (cadr exprs)))
      (fn _ _ _ _ (invalid-name (car exprs)))))))

(def match-macro-expr match-def-expr)
