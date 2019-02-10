;;; Symbol.m
;;;
;;; An implementation of symbols which are encoded using a list of characters.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of Unicode strings.

;; Converts a symbol to a list of characters.
(def symbol->list id)

;; Tests if two symbols are equal.
(def symbol.= (list.= char.=))

;; Adds two symbols.
(def symbol.+ concat)