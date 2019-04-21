;;; Let.m

;; Macro for defining local variables.
(macrofn let exprs
  (if (nil? (cdr exprs)) (macro/list exprs)
    (apply-vararg macro/list
      (macro/symbol (symbol with))
      (cadr exprs)
      (apply-vararg macro/list
        (macro/symbol (symbol fn))
        (car exprs)
        (macro/list
          (cons 
            (macro/symbol (symbol let))
            (cddr exprs)))))))