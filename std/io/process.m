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

;; Sequences a list of processes to a process of a list.
(defn process/sequence list
  (do head (car list)
      tail (process/sequence (cdr list))
    (impure (cons head tail))))

;; Macro for running processes.
(macrofn do env exprs
  (result/success
    (if (nil? (cdr exprs)) (expr/list exprs)
      (apply-vararg expr/list
        (expr/symbol (symbol then-run-with))
        (cadr exprs)
        (apply-vararg expr/list
          (expr/symbol (symbol fn))
          (car exprs)
          (expr/list
            (cons
              (expr/symbol (symbol do))
              (cddr exprs))))))))