;;; Error.m

;; Creates an M error given a message, a function which ignores its argument
;; and returns itself.
(defn error message
  (delay (error message)))