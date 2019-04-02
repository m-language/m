;;; Ostream.m

;; Writes a character to an ostream.
(def ostream.write
  (fn ostream char
    (ostream char)))

;; Writes a line to an ostream.
(def ostream.writeln
  (fn ostream line
    (if (nil? line)
      (ostream.write ostream linefeed)
      (then-run
        (ostream.write ostream (car line))
        (ostream.writeln ostream (cdr line))))))