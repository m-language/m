#(Module for using M processes)
(defmodule process {
  #(Transforms a process with a function)
  (def do do@process)

  #(Creates a process from a value)
  (def impure impure@process)

  #(Combines two processes)
  (defn (combine-process a b)
    (do a result b))
})
