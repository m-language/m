;;; Lazy.m

;; Macro which delays the evaluation of an expression.
(macro delay
  (fn exprs
    (macro/list
      (cons (macro/symbol (symbol fn))
      (cons (macro/symbol (symbol _))
        exprs)))))

;; Macro which forces the delayed evaluation of an expression.
(macro force
  (fn exprs
    (apply-vararg macro/list (car exprs) (macro/list ()))))