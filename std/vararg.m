;;; Vararg.m

;; Applies a function to a variadic number of arguments.
(macro apply-vararg
  (fn exprs
    (list-expr0
      (list
        (car exprs)
        (list-expr0
          (cons
            (symbol-expr0 (symbol list))
            (cdr exprs)))))))