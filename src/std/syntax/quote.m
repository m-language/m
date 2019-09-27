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
              (fn symbol
                (if (symbol.= symbol (symbol unquote))
                  (cadr exprs)
                  (quote-list exprs)))
              (fn _ (quote-list exprs)))))))))

(defn quote-symbol symbol
  (apply-vararg expr/list
    (expr/symbol (symbol expr/symbol))
    (apply-vararg expr/list
      (expr/symbol (symbol symbol))
      (expr/symbol symbol))))

(defn quote-list exprs
  (apply-vararg expr/list
    (expr/symbol (symbol expr/cons))
    (apply-vararg expr/list
      (expr/symbol (symbol quote))
      (car exprs))
    (apply-vararg expr/list
      (expr/symbol (symbol quote))
      (expr/list (cdr exprs)))))
