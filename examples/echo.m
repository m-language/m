#(A process which writes stdin to stdout)
(def echo
  (cont char stdin
    ((process combine) (stdout char) echo)))
