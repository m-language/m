;; The main function for M.
(defn """" args
  (let file (file.child file.local-file (car args))
    (do exprs (parse-file file)
        result (generate exprs)
      (repl
        (generated.global-env result)
        (interpret-declarations (generated.declarations result) empty-heap)
        nat.1
        (empty-tree-map compare-symbol)))))

;; Compiles a file.
(defn compile backend in out
  (do exprs (parse-file in)
      result (generate exprs) 
    (write-result backend result out)))

;; Generates list of expressions.
(defn generate exprs
  (mpm-resolve-generate-result (generate-exprs exprs default-global-env)))

;; Writes a generate result.
(defn write-result backend result out
  (generate-result.match result
  (fn degenerate' (impure (error (car (degenerate.errors degenerate')))))
  (fn generating' (impure (error (flat-map (generating.dependencies generating') ((swap append) space)))))
  (fn generated' (backend out (generated.operation generated') (generated.declarations generated')))))

(def mc:test (delay success))