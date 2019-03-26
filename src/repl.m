(def debug ())

;; The M repl.
(def repl
  (fn global-env
  (fn heap
  (fn index
  (fn resolved
    (then-run-with
      (then-run
        (ostream.write stdout (car (symbol >)))
        (istream.readln stdin))
    (fn line
      (if (symbol.= (filter line (compose not whitespace?)) (symbol ""))
        (impure ())
        (with (char.= (car line) (car (symbol "!")))
        (fn !?
          (with (repl-parse (concat (symbol "(def it ") (append (if !? (cdr line) line) close-parentheses)) index)
          (fn expr
            (then-run-with (mpm-resolve-generate-result' resolved (generate-expr expr default-local-env global-env))
            (fn pair
              (with (repl-interpret-declarations (second pair) heap)
              (fn new-heap
                (with (repl-interpret-operation (second pair) new-heap)
                (fn value
                  ((if !? then-run-with with) value
                  (fn v
                    ((const repl) (debug v)
                      (global-env.with-globals
                        (tree-map.remove (global-env.globals (generated.global-env (second pair))) (symbol "it"))
                        (generated.global-env (second pair)))
                      new-heap
                      (nat.inc index)
                      (first pair))))))))))))))))))))))

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
      (interpret-declarations (generated.declarations result) heap))))

;; Interprets an operation in the repl.
(def repl-interpret-operation
  (fn result
    (fn heap
      (interpret-operation (generated.operation result) heap))))