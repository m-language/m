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
(def open-parentheses (car (symbol "(")))

;; The literal character ")".
(def close-parentheses (car (symbol ")")))

;; The literal character ";".
(def semicolon (car (symbol ";")))

;; The literal character ".".
(def dot (car (symbol ".")))

;; The literal character "\"".
(def quote (car (symbol "\"")))

;; The literal character "\\".
(def backslash (car (symbol "\\")))

;; The literal character " ".
(def space (car (symbol " ")))

;; The literal character "\b".
(def backspace (car (symbol "\b")))

;; The literal character "\t".
(def tab (car (symbol "\t")))

;; The literal character "\n".
(def linefeed (car (symbol "\n")))

;; The literal character "\v".
(def vtab (car (symbol "\v")))

;; The literal character "\f".
(def formfeed (car (symbol "\f")))

;; The literal character "\r".
(def carriage-return (car (symbol "\r")))

;; The literal character "b".
(def letter-b (car (symbol "b")))

;; The literal character "t".
(def letter-t (car (symbol "t")))

;; The literal character "n".
(def letter-n (car (symbol "n")))

;; The literal character "v".
(def letter-v (car (symbol "v")))

;; The literal character "f".
(def letter-f (car (symbol "f")))

;; The literal character "r".
(def letter-r (car (symbol "r")))

;; True if a character is "\r", "\n", or "\f".
(def newline?
  (fn char
    (or (char.= char linefeed)
        (fn ""
          (or (char.= char carriage-return)
              (fn ""
                (char.= char formfeed)))))))

;; True if a character is a newline, " ", "\t", or "\v".
(def whitespace?
  (fn char
    (or (newline? char)
        (fn ""
          (or (char.= char space)
              (fn ""
                (or (char.= char tab)
                    (fn ""
                      (char.= char vtab)))))))))