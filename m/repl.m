;; The M repl.
(defnrec repl global-env heap index name resolved
  (do line
    (then-run
      (ostream.write stdout (car (symbol >)))
      (istream.readln stdin))
    (if (symbol.= (filter line (compose not whitespace?)) nil)
      (impure nil)
      (let !? (char.= (car line) (car (symbol "!")))
           text (concat (symbol "(def ") 
                  (concat (append name space) 
                    (append (if !? (cdr line) line) close-parentheses)))
           expr (repl-parse text index)
        (do pair (mpm-resolve-generate-result' resolved (generate-expr expr default-local-env global-env))
          (generate-result.match (second pair)
          (fn degenerate' 
            (then-run
              (ostream.writeln stdout (car (degenerate.errors degenerate')))
              (repl global-env heap (nat.inc index) resolved)))
          (fn generating'
            (then-run
              (ostream.writeln stdout (flat-map (generating.dependencies generating') ((swap append) space)))
              (repl global-env heap (nat.inc index) resolved)))
          (fn generated'
            (let new-heap (repl-interpret-declarations generated' heap)
                 value (repl-interpret-operation generated' new-heap)
              ((if !? then-run-with with) value
              (fn v
                ((const repl) (debug v)
                  (generated.global-env generated')
                  new-heap
                  (nat.inc index)
                  (cons slash name)
                  (first pair))))))))))))

;; Parses a line in the repl.
(defn repl-parse line index
  (parse-expr
    (mangle-fn-name (symbol "repl") index)
    (append line linefeed)
    (position index nat.1)
    (fn expr path input position
      expr)))

;; Interprets declarations in the repl.
(defn repl-interpret-declarations result heap
  (interpret-declarations (generated.declarations result) heap))

;; Interprets an operation in the repl.
(defn repl-interpret-operation result heap
  (interpret-operation (generated.operation result) heap))