#(Module for writing to an output)
(defmodule output {
  (import process)

  #(Writes a newline to an output)
  (defn (write-newline out)
    (out newline))

  #(Writes a fold of characters to an output)
  (defn (write-fold fold out)
    (fold (impure ' ')
      (fn [acc char]
        (combine acc (out char)))))
  
  #(Writes a line to an output)
  (defn (write-line out->process out)
    (combine (out->process out) (write-newline out)))
})
