;; The main function for M.
(defn "" args
  (let mode (car args)
    (pcond (symbol.= mode)
      (symbol compile) (run-compile (cdr args))
      (error (concat (symbol "Could not find mode ") mode)))))
