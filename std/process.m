;;; Process.m

;; Creates a constant process.
(defn impure x f
  (f x))

;; Runs a function which produces a process in a process, then combines them.
(defn then-run-with p f
  (p f))

;; Combines two processes, running them one after another.
(defn then-run a b
  (then-run-with a (const b)))

;; Runs a function in a process.
(defn run-with p f
  (then-run-with p (compose impure f)))

;; Macro for running processes.
(macrofn do exprs
  (if (nil? (cdr exprs)) (list-expr0 exprs)
    (apply-vararg list-expr0
      (symbol-expr0 (symbol then-run-with))
      (cadr exprs)
      (apply-vararg list-expr0
        (symbol-expr0 (symbol fn))
        (car exprs)
        (list-expr0
          (cons
            (symbol-expr0 (symbol do))
            (cddr exprs)))))))