;; The main function for the M compiler.
(def ""
  (fn args
    (if (nil? args)
      (repl (default-env ()) empty-heap nat.1)
      (with (file.child file.local-file (car args))
      (fn in
        (then-run-with (parse-file in () true)
        (fn exprs
          (if (nil? (cdr args))
            (with (generate-env (default-env exprs))
            (fn result
              (repl
                (generate-result.env result)
                (interpret-declarations
                  (generate-result.declarations result)
                  empty-heap)
                nat.1)))
            (with (file.child file.local-file (cadr args))
            (fn out
              (write-result
                (generate-env (default-env exprs))
                out)))))))))))

;; Writes a program.
(def write-program ())

;; Writes a generate result.
(def write-result
  (fn result
    (fn out
      (write-program
        out
        (generate-result.operation result)
        (generate-result.declarations result)))))