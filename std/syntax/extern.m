;;; Extern.m

;; Creates an external definition with a given name.
(macrofn extern env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol def))
      (car exprs)
      (car exprs))))