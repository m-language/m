;;; Istream.m

;; Reads a character from an istream.
(def istream.read
  (fn istream istream))

;; Reads a line from an istream.
(def istream.readline
  (fn istream
    (ap then-run-with istream
    (fn char
      (if (ap newline? char)
        (impure ())
        (ap run-with (ap istream.readline istream)
        (fn line
          (ap cons char line))))))))