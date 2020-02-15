#(A process which writes stdin to stdout)
(import process
  (def echo
    (do stdin
      (fn char
        (combine (stdout char) echo)))))
