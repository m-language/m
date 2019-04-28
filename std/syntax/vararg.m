;;; Vararg.m

;; Applies a function to a variadic number of arguments.
(macro apply-vararg
  (fn env exprs
    (expr/list
      (list
        (car exprs)
        (expr/list
          (cons
            (expr/symbol (symbol list))
            (cdr exprs)))))))