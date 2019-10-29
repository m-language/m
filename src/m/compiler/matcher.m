;; Matches a function expression.
(defn match-fn-expr exprs no-expr invalid-arg success
  (if (nil? exprs) no-expr
    (match-fn-expr' exprs nil invalid-arg success)))

(defn match-fn-expr' exprs names invalid-arg success
  (if (nil? (cdr exprs))
    (success (reverse names) (car exprs))
    (expr.match (car exprs)
      (fn name _ (match-fn-expr' (cdr exprs) (cons name names) invalid-arg success))
      (fn _ _ (invalid-arg (car exprs))))))

;; Matches a def expression.
(defn match-def-expr exprs no-name no-expr extra-exprs invalid-name success
  (if (nil? exprs) no-name
  (if (nil? (cdr exprs)) no-expr
  (if (not (nil? (cddr exprs))) extra-exprs
    (expr.match (car exprs)
      (fn name _  (success name (cadr exprs)))
      (fn _ _ (invalid-name (car exprs))))))))

;; Matches a macro expression.
(def match-macro-expr match-def-expr)

;; Matches a symbol literal expression.
(defn match-symbol-literal-expr exprs no-symbol extra-exprs invalid-symbol success
  (if (nil? exprs) no-symbol
  (if (not (nil? (cdr exprs))) extra-exprs
    (expr.match (car exprs)
      (fn name _ (success name))
      (fn _ _ (invalid-symbol (car exprs)))))))

;; Matches an applicaion expression.
(defn match-apply-expr exprs no-args success
  (if (nil? (cdr exprs)) no-args success))
