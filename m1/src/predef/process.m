;; Combines two processes, running them one after another.
(def then-run ())

;; Runs a function in a process.
(def run-with ())

;; Runs a function which produces a process in a process, then combines them.
(def then-run-with ())

;; Converts a function to a process.
(def function->process ())