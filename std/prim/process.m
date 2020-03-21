#(Module for using M processes)
(defmodule process {
  #(Combines two processes)
  (defn (combine a b)
    (cont result a b))
})
