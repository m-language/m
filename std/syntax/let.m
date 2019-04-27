;;; Let.m

;; Macro for defining local variables.
(macrofn let exprs
  (if (nil? (cdr exprs)) (expr/list exprs)
    (apply-vararg expr/list
      (expr/symbol (symbol with))
      (cadr exprs)
      (apply-vararg expr/list
        (expr/symbol (symbol fn))
        (car exprs)
        (expr/list
          (cons 
            (expr/symbol (symbol let))
            (cddr exprs)))))))