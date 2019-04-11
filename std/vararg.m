;;; Vararg.m

;; Applies a function to a variadic number of arguments.
(macro apply-vararg
  (fn expr
    (list
      (car expr)
      (right
        (cons
          (left (symbol list))
          (cdr expr))))))