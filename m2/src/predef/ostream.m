;;; Ostream.m

;; Writes a character to an ostream.
(def ostream.write
  (fn ostream
    (fn char
      (ap ostream char))))

;; Writes a list of chars to an ostream.
(def ostream.[write]
  (fn ostream
    (fn list
      (if (ap nil? list)
        (impure ())
        (ap then-run
          (ap ostream.write ostream (ap car list))
          (ap ostream.[write] ostream (ap cdr list)))))))

;; Writes a newline to an ostream.
(def ostream.newline
  (fn ostream
    (ap ostream.write ostream linefeed)))

;; Writes a line to an ostream.
(def ostream.writeln
  (fn ostream
    (fn line
      (ap then-run
        (ap ostream.[write] ostream line)
        (ap ostream.newline ostream)))))