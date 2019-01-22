;; The main function for the M compiler.
(def ""
  (lambda args
    (if (nil? args)
      (repl (default-env ()) default-heap)
      (with (file.child file.local-file (car args))
      (lambda in
        (then-run-with (parse-file in () true)
        (lambda exprs
          (if (nil? (cdr args))
            (with
              (generate-env (default-env exprs (empty-tree-map compare-symbol)))
            (lambda result
              (repl
                (generate-result.env result)
                (interpret-declarations
                  (map (generate-result.declarations result)
                    (lambda decl
                      (if (is? (symbol def-declaration) decl)
                         (const decl (debug (def-declaration.name decl)))
                         decl)))
                  default-heap))))
            (with (file.child file.local-file (cadr args))
            (lambda out
              (write-result
                (generate-env (default-env exprs internals))
                out)))))))))))

;; Writes a program.
(def write-program ())

;; Writes a generate result.
(def write-result
  (lambda result
    (lambda out
      (write-program
        out
        (generate-result.operation result)
        (generate-result.declarations result)))))