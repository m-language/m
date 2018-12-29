;; The main function for the M compiler.
(def ""
  (lambda args
    (with (file.child file.local-file (car args))
    (lambda in
      (with (file.child file.local-file (cadr args))
      (lambda out
        (then-run-with (parse-file in () true)
        (lambda exprs
          (generate exprs out)))))))))