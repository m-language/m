;;; Istream.m

;; Reads a character from an istream.
(def istream.read id)

;; Reads a line from an istream.
(defn istream.readln istream
  (do char istream
    (if (char.= linefeed char)
      (impure ())
      (do line (istream.readln istream)
        (impure (if (newline? char) line (cons char line)))))))