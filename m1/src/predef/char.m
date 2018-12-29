;; Converts a character to a nat.
(def char->nat ())

;; Tests if two characters are equal.
(def char.= ())

;; The literal character "(".
(def open-parentheses (symbol->char (symbol "(")))

;; The literal character ")".
(def close-parentheses (symbol->char (symbol ")")))

;; The literal character ";".
(def semicolon (symbol->char (symbol ";")))

;; The literal character ".".
(def dot (symbol->char (symbol ".")))

;; The literal character "\"".
(def quote (symbol->char (symbol "\"")))

;; The literal character "\\".
(def backslash (symbol->char (symbol "\\")))

;; The literal character " ".
(def space (symbol->char (symbol " ")))

;; The literal character "\b".
(def backspace (symbol->char (symbol "\b")))

;; The literal character "\t".
(def tab (symbol->char (symbol "\t")))

;; The literal character "\n".
(def linefeed (symbol->char (symbol "\n")))

;; The literal character "\v".
(def vtab (symbol->char (symbol "\v")))

;; The literal character "\f".
(def formfeed (symbol->char (symbol "\f")))

;; The literal character "\r".
(def carriage-return (symbol->char (symbol "\r")))

;; The literal character "b".
(def letter-b (symbol->char (symbol "b")))

;; The literal character "t".
(def letter-t (symbol->char (symbol "t")))

;; The literal character "n".
(def letter-n (symbol->char (symbol "n")))

;; The literal character "v".
(def letter-v (symbol->char (symbol "v")))

;; The literal character "f".
(def letter-f (symbol->char (symbol "f")))

;; The literal character "r".
(def letter-r (symbol->char (symbol "r")))

;; True if a character is "\r", "\n", or "\f".
(def newline?
  (lambda char
    (or (char.= char linefeed)
        (lambda ""
          (or (char.= char carriage-return)
              (lambda ""
                (char.= char formfeed)))))))

;; True if a character is a newline, " ", "\t", or "\v".
(def whitespace?
  (lambda char
    (or (newline? char)
        (lambda ""
          (or (char.= char space)
              (lambda ""
                (or (char.= char tab)
                    (lambda "" (char.= char vtab)))))))))