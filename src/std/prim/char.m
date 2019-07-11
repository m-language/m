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
(def dot (car (symbol .)))

;; The literal character """.
(def quote (car (symbol """")))

;; The literal character "/".
(def slash (car (symbol "/")))

;; The literal character "\".
(def backslash (car (symbol "\")))

;; The literal character "\s".
(def space (car (symbol " ")))

;; The literal character "\t".
(def tab (car (symbol "	")))

;; The literal character "\n".
(def linefeed (car (symbol "
")))

;; The literal character "\r".
(def carriage-return (car (symbol "
")))

;; True if a character is "\r" or "\n".
(defn newline? char
  (| (char.= char linefeed)
     (char.= char carriage-return)))

;; True if a character is " ", "\t", or a newline.
(defn whitespace? char
  (| (char.= char space)
  (| (char.= char tab)
     (newline? char))))

;; Compares chars.
(defn compare-char a b
  (compare-nat (char->nat a) (char->nat b)))
