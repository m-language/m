;;; Istream.m

;; Reads a character from an istream.
(def istream.read id)

;; Reads a line from an istream.
(defn istream.readln istream
  (do char istream
    (if (newline? char)
      (impure ())
      (do line (istream.readln istream)
        (impure (cons char line))))))