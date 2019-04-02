;;; Process.m

;; Creates a constant process.
(def impure
  (fn x f
    (f x)))

;; Runs a function which produces a process in a process, then combines them.
(def then-run-with
  (fn p f
    (p f)))

;; Combines two processes, running them one after another.
(def then-run
  (fn a b
    (then-run-with a (const b))))

;; Runs a function in a process.
(def run-with
  (fn p f
    (then-run-with p (compose impure f))))