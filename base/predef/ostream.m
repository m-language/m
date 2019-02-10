;;; Ostream.m

;; Writes a character to an ostream.
(def ostream.write
  (fn ostream
    (fn char
      (ostream char))))

;; Writes a list of chars to an ostream.
(def ostream.[write]
  (fn ostream
    (fn list
      (if (nil? list)
        (impure ())
        (then-run
          (ostream.write ostream (car list))
          (ostream.[write] ostream (cdr list)))))))

;; Writes a newline to an ostream.
(def ostream.newline
  (fn ostream
    (ostream.write ostream linefeed)))

;; Writes a line to an ostream.
(def ostream.writeln
  (fn ostream
    (fn line
      (then-run
        (ostream.[write] ostream line)
        (ostream.newline ostream)))))