;;; Vararg.m

;; Applies a function to a variadic number of arguments.
(macro apply-vararg
  (fn expr
    (expr.list
      (list
        (car expr)
        (expr.list
          (cons
            (expr.symbol (symbol list))
            (cdr expr)))))))