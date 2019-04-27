;;; Extern.m

;; Creates an external definition with a given name.
(macrofn extern exprs
  (apply-vararg expr/list
    (expr/symbol (symbol def))
    (car exprs)
    (car exprs)))