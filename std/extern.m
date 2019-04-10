;;; Extern.m
;;;
;;; Allows the definition of external values to be used in M.

;; Creates an external definition with a given name.
(macro extern
  (fn expr
    (list
      (left (symbol def))
      (car expr)
      (car expr))))