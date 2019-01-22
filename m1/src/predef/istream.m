;;; Istream.m

;; Reads a character from an istream.
(def istream.read
  (lambda istream istream))

;; Reads a line from an istream.
(def istream.readline
  (lambda istream
    (then-run-with istream
    (lambda char
      (if (newline? char)
        (do ())
        (run-with (istream.readline istream)
        (lambda line
          (cons char line))))))))