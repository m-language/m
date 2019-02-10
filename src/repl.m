(def debug ())

;; The M repl.
(def repl
  (fn env'
    (fn heap
      (fn index
        (then-run-with
          (then-run
            (ostream.write stdout (car (symbol >)))
            (istream.readline stdin))
        (fn line
          (if (symbol.= line (symbol ""))
            (impure ())
            (with
              (parse-result.expr
                (parse-expr
                  (append line linefeed)
                  (mangle-fn-name (symbol "repl") index)
                  (position index nat.1)))
            (fn expr
              (with (generate-expr expr env')
              (fn result
                (with
                  (interpret-declarations
                    (generate-result.declarations result)
                    heap)
                (fn new-heap
                  (with
                    (debug
                      (interpret-operation
                        (generate-result.operation result)
                        new-heap))
                  (fn value
                    (repl (generate-result.env result) new-heap (nat.inc index)))))))))))))))))