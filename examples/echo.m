#(A process which writes stdin to stdout)
(def echo
  (let-cont char stdin
    ((process combine) (stdout char) echo)))