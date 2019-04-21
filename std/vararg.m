;;; Vararg.m

;; Applies a function to a variadic number of arguments.
(macro apply-vararg
  (fn exprs
    (macro/list
      (list
        (car exprs)
        (macro/list
          (cons
            (macro/symbol (symbol list))
            (cdr exprs)))))))