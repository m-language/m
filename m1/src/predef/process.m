;;; Process.m

;; Creates a constant process.
(def return
  (fn x
    (impure x)))

;; Runs a function which produces a process in a process, then combines them.
(def then-run-with
  (fn p
    (fn f
      (ap p f))))

;; Combines two processes, running them one after another.
(def then-run
  (fn p1
    (fn p2
      (ap then-run-with p1 (ap const p2)))))

;; Runs a function in a process.
(def run-with
  (fn p
    (fn f
      (ap then-run-with p (ap compose return f)))))