;;; Process.m

;; Creates a constant process.
(def return
  (lambda x
    (do x)))

;; Runs a function which produces a process in a process, then combines them.
(def then-run-with
  (lambda p
    (lambda f
      ((do p) id f))))

;; Combines two processes, running them one after another.
(def then-run
  (lambda p1
    (lambda p2
      (then-run-with p1 (const p2)))))

;; Runs a function in a process.
(def run-with
  (lambda p
    (lambda f
      (then-run-with p (compose return f)))))