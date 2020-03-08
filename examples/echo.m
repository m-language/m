#(A process which writes stdin to stdout)
(import process
  (def echo
    (cont char stdin
      (combine (stdout char) echo))))
