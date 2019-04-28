;;; Lazy.m

;; Macro which delays the evaluation of an expression.
(macro delay
  (fn env exprs
    (expr/list
      (cons (expr/symbol (symbol fn))
      (cons (expr/symbol (symbol _))
        exprs)))))

;; Macro which forces the delayed evaluation of an expression.
(macro force
  (fn env exprs
    (apply-vararg expr/list (car exprs) (expr/list ()))))