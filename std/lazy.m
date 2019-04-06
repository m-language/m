;;; Lazy.m

;; Macro which delays the evaluation of an expression.
(macro delay
  (fn expr
    (cons (left (symbol fn))
    (cons (left (symbol _))
      expr))))

;; Macro which forces the delayed evaluation of an expression.
(macro force
  (fn expr
    (cons (car expr) (list1 (right ())))))