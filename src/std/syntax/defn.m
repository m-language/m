;;; Defn.m
;;;
;;; Macros for creating function definitions.

;; Macro for defining macro functions.
(macro macrofn
  (fn env exprs
    (result/success
      (apply-vararg expr/list
        (expr/symbol (symbol macro))
        (car exprs)
        (expr/list
          (cons (expr/symbol (symbol fn))
            (cdr exprs)))))))

;; Macro for defining functions.
(macrofn defn env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol def))
      (car exprs)
      (expr/list
        (cons (expr/symbol (symbol fn))
          (cdr exprs))))))
