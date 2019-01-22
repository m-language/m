;; The M repl.
(def repl
  (lambda env'
    (lambda heap
      (then-run-with
        (then-run
          (ostream.write stdout (car (symbol >)))
          (istream.readline stdin))
      (lambda line
        (if (symbol.= line (symbol ""))
          (do ())
          (with
            (parse-result.expr
              (parse-expr
                (append line linefeed)
                (symbol "repl.m")
                (position nat.1 nat.1)))
          (lambda expr
            (with (generate-expr expr env')
            (lambda result
              (with
                (interpret-declarations
                  (generate-result.declarations result)
                  heap)
              (lambda new-heap
                (with
                  (debug (interpret-operation
                    (generate-result.operation result)
                    ()
                    new-heap))
                  (lambda value
                    (repl (generate-result.env result) new-heap)))))))))))))))