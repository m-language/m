;;; Extern.m

;; Creates an external definition with a given name.
(macrofn extern exprs
  (apply-vararg macro/list
    (macro/symbol (symbol def))
    (car exprs)
    (car exprs)))