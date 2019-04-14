;; The main function for the M compiler.
(defn """" args
  (if (nil? args) empty-repl
  (let in (file.child file.local-file (car args))
    (if (nil? (cdr args)) (run-repl in)
    (let out (file.child file.local-file (cadr args))
      (if (nil? (cddr args)) (compile in out)
      (error (symbol "Usage: mc <in> <out>"))))))))

;; Runs the m repl with no declarations.
(def empty-repl
  (repl default-global-env empty-heap nat.1 (empty-tree-map compare-symbol)))

;; Runs the m repl.
(defn run-repl in
  (then-run-with (generate in)
  (fn result
    (repl
      (generated.global-env result)
      (interpret-declarations (generated.declarations result) empty-heap)
      nat.1
      (empty-tree-map compare-symbol)))))

;; Compiles a file and writes its result.
(defn compile in out
  (then-run-with (generate in)
  (fn result
    (generate-result.match result
    (fn degenerate' (error (car (degenerate.errors degenerate'))))
    (fn generating' (error (flat-map (generating.dependencies generating') ((swap append) space))))
    (fn generated' (write-result generated' out))))))

;; Generates a file.
(defn generate in
  (then-run-with (parse-file in () true)
  (fn exprs
    (mpm-resolve-generate-result (generate-exprs exprs default-global-env)))))

;; Writes a generate result.
(defn write-result result out
  (write-program out
    (generated.operation result)
    (generated.declarations result)))

;; Writes a program.
(extern write-program)

;; Tests the M compiler.
(def mc:test
  (apply-vararg combine-tests 
    bool:test
    char:test
    either:test
    lazy:test
    nat:test))