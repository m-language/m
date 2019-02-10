;;; Istream.m

;; Reads a character from an istream.
(def istream.read
  (fn istream istream))

;; Reads a line from an istream.
(def istream.readline
  (fn istream
    (then-run-with istream
    (fn char
      (if (newline? char)
        (impure ())
        (run-with (istream.readline istream)
        (fn line
          (cons char line))))))))