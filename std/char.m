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

;; The literal character "/".
(def slash (car (symbol "/")))

;; The literal character "\\".
(def backslash (car (symbol "\\")))

;; The literal character " ".
(def space (car (symbol " ")))

;; The literal character "\t".
(def tab (car (symbol "\t")))

;; The literal character "\n".
(def linefeed (car (symbol "\n")))

;; The literal character "\r".
(def carriage-return (car (symbol "\r")))

;; The literal character "t".
(def letter-t (car (symbol "t")))

;; The literal character "n".
(def letter-n (car (symbol "n")))

;; The literal character "r".
(def letter-r (car (symbol "r")))

;; True if a character is "\n".
(def newline?
  (fn char
    (| (char.= char linefeed)
       (char.= char carriage-return))))

;; True if a character is " ", "\t", or a newline.
(def whitespace?
  (fn char
    (| (char.= char space)
    (| (char.= char tab)
       (newline? char)))))