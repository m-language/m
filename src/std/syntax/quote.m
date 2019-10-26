;; Quotes an expression.
(macrofn quote env exprs
  (if (or (nil? exprs) (delay (not (nil? (cdr exprs)))))
    (result/error (symbol "Could not match [expr] => (quote expr)"))
    (result/success
      (expr/match (car exprs)
        (fn symbol (quote-symbol symbol))
        (fn exprs
          (if (nil? exprs)
            (expr/symbol (symbol expr/nil))
            (expr/match (car exprs)
              (fn name
                (if (symbol.= name (symbol unquote))
                  (cadr exprs)
                  (quote-list exprs)))
              (fn list
                (expr/match (car list)
                  (fn name
                    (if (symbol.= name (symbol splice))
                      (quote-splice (cadr list) exprs)
                      (quote-list exprs)))
                  (fn _ (quote-list exprs)))))))))))

(defn quote-symbol name
  (apply-vararg expr/list
    (expr/symbol (symbol expr/symbol))
    (apply-vararg expr/list
      (expr/symbol (symbol symbol))
      (expr/symbol name))))

(defn quote-list exprs
  (apply-vararg expr/list
    (expr/symbol (symbol expr/cons))
    (apply-vararg expr/list
      (expr/symbol (symbol quote))
      (car exprs))
    (apply-vararg expr/list
      (expr/symbol (symbol quote))
      (expr/list (cdr exprs)))))

(defn quote-splice spliced exprs
  (apply-vararg expr/list
    (expr/symbol (symbol expr/concat-list))
    (apply-vararg expr/list
      (expr/symbol (symbol quote))
      (expr/list (cdr exprs)))
    spliced))
