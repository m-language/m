;;; Let.m

;; Macro for defining local variables.
(macrofn let env exprs
  (result/success
    (if (nil? (cdr exprs)) (car exprs)
      (apply-vararg expr/list
        (expr/symbol (symbol with))
        (cadr exprs)
        (apply-vararg expr/list
          (expr/symbol (symbol fn))
          (car exprs)
          (expr/list
            (cons 
              (expr/symbol (symbol let))
              (cddr exprs))))))))