;;; Char.m
;;;
;;; An implementation of characters which are encoded using a natural number as
;;; the character's Unicode value.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of Unicode characters.

;; Converts a character to a natural number.
(def char->nat id)

;; Tests if two characters are equal.
(def char.= nat.=)

;; The literal character "(".
(def open-parentheses (ap car (symbol "(")))

;; The literal character ")".
(def close-parentheses (ap car (symbol ")")))

;; The literal character ";".
(def semicolon (ap car (symbol ";")))

;; The literal character ".".
(def dot (ap car (symbol ".")))

;; The literal character "\"".
(def quote (ap car (symbol "\"")))

;; The literal character "\\".
(def backslash (ap car (symbol "\\")))

;; The literal character " ".
(def space (ap car (symbol " ")))

;; The literal character "\b".
(def backspace (ap car (symbol "\b")))

;; The literal character "\t".
(def tab (ap car (symbol "\t")))

;; The literal character "\n".
(def linefeed (ap car (symbol "\n")))

;; The literal character "\v".
(def vtab (ap car (symbol "\v")))

;; The literal character "\f".
(def formfeed (ap car (symbol "\f")))

;; The literal character "\r".
(def carriage-return (ap car (symbol "\r")))

;; The literal character "b".
(def letter-b (ap car (symbol "b")))

;; The literal character "t".
(def letter-t (ap car (symbol "t")))

;; The literal character "n".
(def letter-n (ap car (symbol "n")))

;; The literal character "v".
(def letter-v (ap car (symbol "v")))

;; The literal character "f".
(def letter-f (ap car (symbol "f")))

;; The literal character "r".
(def letter-r (ap car (symbol "r")))

;; True if a character is "\r", "\n", or "\f".
(def newline?
  (fn char
    (ap or (ap char.= char linefeed)
        (fn ""
          (ap or (ap char.= char carriage-return)
              (fn ""
                (ap char.= char formfeed)))))))

;; True if a character is a newline, " ", "\t", or "\v".
(def whitespace?
  (fn char
    (ap or (ap newline? char)
        (fn ""
          (ap or (ap char.= char space)
              (fn ""
                (ap or (ap char.= char tab)
                    (fn ""
                      (ap char.= char vtab)))))))))