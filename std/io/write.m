#(Module for writing to an output)
(defmodule write {
  (import process)

  #(Writes a newline to an output)
  (defn (write-newline output)
    (output newline))

  #(Writes a fold of characters to an output)
  (defn (write-fold fold output)
    (fold (impure ' ')
      (fn [acc char]
        (combine acc (output char)))))
  
  #(Writes a line to an output)
  (defn (write-line output->process output)
    (combine (output->process output) (write-newline output)))
})
