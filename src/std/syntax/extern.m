;;; Extern.m

;; Creates an external definition with a given name.
(macrofn extern env exprs
  (result/success
    (quote
      (def (unquote (car exprs))
           (unquote (car exprs))))))