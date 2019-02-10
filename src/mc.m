;; The main function for the M compiler.
(def ""
  (fn args
    (if (ap nil? args)
      (ap repl (ap default-env ()) empty-heap nat.1)
      (ap with (ap file.child file.local-file (ap car args))
      (fn in
        (ap then-run-with (ap parse-file in () true)
        (fn exprs
          (if (ap nil? (ap cdr args))
            (ap with (ap generate-env (ap default-env exprs))
            (fn result
              (ap repl
                (ap generate-result.env result)
                (ap interpret-declarations
                  (ap generate-result.declarations result)
                  empty-heap)
                nat.1)))
            (ap with (ap file.child file.local-file (ap cadr args))
            (fn out
              (ap write-result
                (ap generate-env (ap default-env exprs))
                out)))))))))))

;; Writes a program.
(def write-program ())

;; Writes a generate result.
(def write-result
  (fn result
    (fn out
      (ap write-program
        out
        (ap generate-result.operation result)
        (ap generate-result.declarations result)))))