;;; Error.m
;;;
;;; An implementation of errors as a function which ignores its argument and
;;; returns itself.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of errors, and any errors in the backend are represented
;;; with these definitions.

;; Creates an M error given a message.
(defn error message
  (delay (error message)))