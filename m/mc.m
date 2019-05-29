;; The main function for M.
(defn """" args
  (let mode (car args)
    (pcond (symbol.= mode)
      (symbol repl) (run-repl (cdr args))
      (symbol compile) (run-compile (cdr args))
      (symbol mpm-put) (run-mpm-put (cdr args))
      (error (concat (symbol "Could not find mode ") mode)))))

;; Runs the M repl.
(defn run-repl args
  (let file (file.child file.local-file (car args))
    (do exprs (parse-file file)
        result (generate exprs)
      (repl
        (generated.global-env result)
        (interpret-declarations (generated.declarations result) empty-heap)
        nat.1
        (empty-tree-map compare-symbol)))))

;; Runs the M compiler.
(defn run-compile args
  (let backend (get-backend (car args))
       in (file.child file.local-file (cadr args))
       out (file.child file.local-file (caddr args))
    (do exprs (parse-file in)
        result (generate exprs)
      (write-result backend result out))))

;; Runs mpm-put.
(defn run-mpm-put args
  (let file (file.child file.local-file (car args))
    (do exprs (parse-file file)
        result (generate exprs)
      (then-run
        (mpm-put-refs (generated.declarations result))
        (mpm-put-srcs file)))))
  
;; Gets the backend given a name.
(defn get-backend name
  (pcond (symbol.= name)
    (symbol m) m-backend
    (symbol jvm) jvm-backend
    (error (concat (symbol "Could not find backend ") name))))

;; Generates list of expressions.
(defn generate exprs
  (mpm-resolve-generate-result (generate-exprs exprs default-global-env)))

;; Writes a generate result.
(defn write-result backend result out
  (generate-result.match result
  (fn degenerate' (impure (error (car (degenerate.errors degenerate')))))
  (fn generating' (impure (error (flat-map (generating.dependencies generating') ((swap append) space)))))
  (fn generated' (backend out (generated.operation generated') (generated.declarations generated')))))

(def m:test (delay success))
