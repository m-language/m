(include predef)
(include generator)

;; Compiles a map of names to files.
(def compile
  (lambda files
    (tree-map.fold local-files
      (function->process (lambda unused true))
      (lambda process
        (lambda name
          (lambda expr
            (then-run process (generate name out-file expr))))))))

(run-unsafe (compile local-files))