;; The main function for M.
(defn "" args
  (let mode (car args)
    (pcond (symbol.= mode)
      (symbol compile) (run-compile (cdr args))
      (error (concat (symbol "Could not find mode ") mode)))))

;; Runs the M compiler.
(defn run-compile args
  (let backend (get-backend (car args))
       in (file.child file.local-file (cadr args))
       out (file.child file.local-file (caddr args))
    (do exprs (parse-file in)
        result (generate exprs)
      (write-result backend result out))))
  
;; Gets the backend given a name.
(defn get-backend name
  (pcond (symbol.= name)
    (symbol m) m-backend
    (symbol jvm) jvm-backend
    (error (concat (symbol "Could not find backend ") name))))

;; Generates list of expressions.
(defn generate exprs
  (impure (generate-exprs exprs default-global-env)))

;; Writes a generate result.
(defn write-result backend result out
  (generate-result.match result
  (fn degenerate' (impure (error (car (degenerate.errors degenerate')))))
  (fn generating' (impure (error (flat-map (generating.dependencies generating') ((swap append) space)))))
  (fn generated' (backend out (generated.operation generated') (generated.declarations generated')))))
