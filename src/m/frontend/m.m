;; Runs the M compiler.
(defn run-compile args
  (let backend (get-backend (car args))
       in (file.child file.local-file (cadr args))
       out (file.child file.local-file (caddr args))
    (do exprs (parse-file in)
        result (generate exprs)
      (write-result backend result in out))))
  
;; Gets the backend given a name.
(defn get-backend name
  (pcond (symbol.= name)
    (symbol m) m-backend'
    (symbol jvm) jvm-backend'
    (symbol js) js-backend'
    (error (concat (symbol "Could not find backend ") name))))

;; Generates list of expressions.
(defn generate exprs
  (impure (generate-exprs exprs default-global-env)))

;; Writes a generate result.
(defn write-result backend result in out
  (generate-result.match result
  (fn degenerate' (ostream.writeln stderr (concat (symbol "Error: ") (car (degenerate.errors degenerate')))))
  (fn generating' (error (flat-map (generating.dependencies generating') ((swap append) space))))
  (fn generated'
    (let unresolved (global-env.unresolved (generated.global-env generated'))
      (if (nil? unresolved)
        (backend in out (generated.trees generated'))
        (ostream.writeln stderr
          (flat-map unresolved
            (fn name
              (concat (symbol "Error: ") 
              (concat name (append (symbol " is not defined") linefeed)))))))))))
