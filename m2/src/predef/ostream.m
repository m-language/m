;;; Ostream.m

;; Writes a character to an ostream.
(def ostream.write
  (lambda ostream
    (lambda char
      (ostream char))))

;; Writes a list of chars to an ostream.
(def ostream.[write]
  (lambda ostream
    (lambda list
      (if (nil? list)
        (do ())
        (then-run
          (ostream.write ostream (car list))
          (ostream.[write] ostream (cdr list)))))))

;; Writes a newline to an ostream.
(def ostream.newline
  (lambda ostream
    (ostream.write ostream linefeed)))

;; Writes a line to an ostream.
(def ostream.writeln
  (lambda ostream
    (lambda line
      (then-run
        (ostream.[write] ostream line)
        (ostream.newline ostream)))))