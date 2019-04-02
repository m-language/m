;; The main function for the M compiler.
(def ""
  (fn args
    (if (nil? args) (empty-repl ())
    (with (file.child file.local-file (car args))
    (fn in
      (if (nil? (cdr args)) (run-repl in)
      (with (file.child file.local-file (cadr args))
      (fn out
        (if (nil? (cddr args)) (compile in out)
        (error (symbol "Usage: mc <in> <out>")))))))))))

;; Runs the m repl with no declarations.
(def empty-repl
  (fn ""
    (repl default-global-env empty-heap nat.1 (empty-tree-map compare-symbol))))

;; Runs the m repl.
(def run-repl
  (fn in
    (then-run-with (generate in)
    (fn result
      (repl
        (generated.global-env result)
        (interpret-declarations (generated.declarations result) empty-heap)
        nat.1
        (empty-tree-map compare-symbol))))))

;; Compiles a file and writes its result.
(def compile
  (fn in out
    (then-run-with (generate in)
    (fn result
      (generate-result.match result
      (fn degenerate' (error (car (degenerate.errors degenerate'))))
      (fn generating' (error (flat-map (generating.dependencies generating') ((swap append) space))))
      (fn generated' (write-result generated' out)))))))

;; Generates a file.
(def generate
  (fn in
    (then-run-with (parse-file in () true)
    (fn exprs
      (mpm-resolve-generate-result (generate-exprs exprs default-global-env))))))

;; Writes a generate result.
(def write-result
  (fn result out
    (write-program out
      (generated.operation result)
      (generated.declarations result))))

;; Writes a program.
(def write-program ())

;; Tests the M compiler.
(def mc-test
  (impure ()))