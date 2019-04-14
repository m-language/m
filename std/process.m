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
(macrofn do expr
  (if (nil? (cdr expr)) (expr.list expr)
    (apply-vararg expr.list
      (expr.symbol (symbol then-run-with))
      (cadr expr)
      (apply-vararg expr.list
        (expr.symbol (symbol fn))
        (car expr)
        (expr.list
          (cons
            (expr.symbol (symbol do))
            (cddr expr)))))))