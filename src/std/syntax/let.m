;;; Let.m

;; Macro for defining local variables.
(macrofn let env exprs
  (result/success
    (if (nil? (cdr exprs)) (car exprs)
      (quote
        (with (unquote (cadr exprs))
          (fn (unquote (car exprs))
            (let (splice (cddr exprs)))))))))