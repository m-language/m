;;; Istream.m

;; Reads a character from an istream.
(def istream.read id)

;; Reads a line from an istream.
(defn istream.readln istream
 (then-run-with istream
 (fn char
   (if (newline? char)
     (impure ())
     (run-with (istream.readln istream)
     (fn line
       (cons char line)))))))