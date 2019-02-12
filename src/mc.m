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
        (error (symbol "Too many arguments.")))))))))))

;; Runs the m repl with no declarations.
(def empty-repl
  (fn ""
    (repl (default-env ()) empty-heap nat.1)))

;; Runs the m repl.
(def run-repl
  (fn in
    (then-run-with (generate in)
    (fn result
      (repl
        (generate-result.env result)
        (interpret-declarations (generate-result.declarations result) empty-heap)
        nat.1)))))

;; Compiles a file and writes its result.
(def compile
  (fn in
    (fn out
      (then-run-with (generate in)
      (fn result
        (write-result result out))))))

;; Puts a file in mpm.
(def mpm-put
  (fn in
    (then-run-with (generate in)
    (fn result
      (then-run
        (mpm-put-refs (generate-result.declarations result))
        (mpm-put-srcs in))))))

;; Generates a file.
(def generate
  (fn in
    (then-run-with (parse-file in () true)
    (fn exprs
      (generate-env (default-env exprs))))))

;; Writes a program.
(def write-program ())

;; Writes a generate result.
(def write-result
  (fn result
    (fn out
      (write-program
        out
        (generate-result.operation result)
        (generate-result.declarations result)))))