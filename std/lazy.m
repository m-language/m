;;; Lazy.m

;; Macro which delays the evaluation of an expression.
(macro delay
  (fn exprs
    (list-expr0
      (cons (symbol-expr0 (symbol fn))
      (cons (symbol-expr0 (symbol _))
        exprs)))))

;; Macro which forces the delayed evaluation of an expression.
(macro force
  (fn exprs
    (apply-vararg list-expr0 (car exprs) (list-expr0 ()))))