(def debug ())

;; The M repl.
(def repl
  (fn env'
    (fn heap
      (fn index
        (then-run-with
          (then-run
            (ostream.write stdout (car (symbol >)))
            (istream.readln stdin))
        (fn line
          (if (symbol.= line (symbol ""))
            (impure ())
            (with (char.= (car line) (car (symbol "!")))
            (fn !?
              (with (repl-parse (if !? (cdr line) line) index)
              (fn expr
                (then-run-with (generate-expr expr env')
                (fn result
                  (with (repl-interpret-declarations result heap)
                  (fn new-heap
                    (with (repl-interpret-operation result new-heap)
                    (fn value
                      ((if !? then-run-with with) value
                      (fn v
                        ((const repl) (debug v)
                           (generate-result.env result) new-heap (nat.inc index)))))))))))))))))))))

;; Parses a line in the repl.
(def repl-parse
  (fn line
    (fn index
      (parse-result.expr
        (parse-expr
          (append line linefeed)
          (mangle-fn-name (symbol "repl") index)
          (position index nat.1))))))

;; Interprets declarations in the repl.
(def repl-interpret-declarations
  (fn result
    (fn heap
      (interpret-declarations (generate-result.declarations result) heap))))

;; Interprets an operation in the repl.
(def repl-interpret-operation
  (fn result
    (fn heap
      (interpret-operation (generate-result.operation result) heap))))