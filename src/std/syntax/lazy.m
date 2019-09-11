;;; Lazy.m

;; Macro which delays the evaluation of an expression.
(macro delay
  (fn env exprs
    (result/success
      (expr/list
        (cons (expr/symbol (symbol fn))
        (cons (expr/symbol (symbol ""))
          exprs))))))

;; Macro which forces the delayed evaluation of an expression.
(macro force
  (fn env exprs
    (result/success
      (apply-vararg expr/list 
        (car exprs) 
        (expr/symbol (symbol nil))))))