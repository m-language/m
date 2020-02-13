#(Module for using M processes)
(defmodule process {
  (import process-ops)

  #(Combines two processes)
  (defn (combine a b)
    (do a (fn result b)))
})
