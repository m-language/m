;;; Istream.m

;; Reads a character from an istream.
(def istream.read
  (fn istream istream))

;; Reads a line from an istream.
(def istream.readln
  (fn istream
    (then-run-with istream
    (fn char
      (if (newline? char)
        (impure ())
        (run-with (istream.readln istream)
        (fn line
          (cons char line))))))))