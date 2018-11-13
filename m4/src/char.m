;; Converts a character to a nat.
(def char->nat ())

;; Tests if two characters are equal.
(def eq-char ())

(import symbol)

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

(import bool)

;; True if a character is "\r", "\n", or "\f".
(def is-newline
  (lambda char
    (or (eq-char char linefeed)
        (lambda unused
          (or (eq-char char carriage-return)
              (lambda unused
                (eq-char char formfeed)))))))

;; True if a character is a newline, " ", "\t", or "\v".
(def is-whitespace
  (lambda char
    (or (is-newline char)
        (lambda unused
          (or (eq-char char space)
              (lambda unused
                (or (eq-char char tab)
                    (lambda unusedd (eq-char char vtab)))))))))

;; True if a character is part of an identifier.
(def is-identifier-character
  (lambda char
    (not
      (or (is-whitespace char)
          (lambda unused
            (or (eq-char char open-parentheses)
                (lambda unused
                  (eq-char char close-parentheses))))))))

;; Maps an escape code to its character.
(def escape-map
  (lambda char
    (if (eq-char char letter-b) backspace
    (if (eq-char char letter-t) tab
    (if (eq-char char letter-n) linefeed
    (if (eq-char char letter-v) vtab
    (if (eq-char char letter-f) formfeed
    (if (eq-char char letter-r) carriage-return
      char))))))))