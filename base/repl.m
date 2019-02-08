;; The M repl.
(def repl
  (fn env'
    (fn heap
      (ap then-run-with
        (ap then-run
          (ap ostream.write stdout (ap car (symbol >)))
          (ap istream.readline stdin))
      (fn line
        (if (ap symbol.= line (symbol ""))
          (impure ())
          (ap with
            (ap parse-result.expr
              (ap parse-expr
                (ap append line linefeed)
                (symbol "repl.m")
                (ap position nat.1 nat.1)))
          (fn expr
            (ap with (ap generate-expr expr env')
            (fn result
              (ap with
                (ap interpret-declarations
                  (ap generate-result.declarations result)
                  heap)
              (fn new-heap
                (ap with
                  (ap interpret-operation
                    (ap generate-result.operation result)
                    ()
                    new-heap)
                (fn value
                  (ap then-run
                    (ap ostream.writeln stdout (symbol "<function>"))
                    (ap repl (ap generate-result.env result) new-heap))))))))))))))))