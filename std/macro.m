;;; Macro.m
;;;
;;; Various macros which have no external dependencies.

;; Macro for defining macro functions.
(macro macrofn
  (fn expr
    (apply-vararg expr.list
      (expr.symbol (symbol macro))
      (car expr)
      (expr.list
        (cons 
          (expr.symbol (symbol fn))
          (cdr expr))))))

;; Macro for defining functions.
(macrofn defn expr
  (apply-vararg expr.list
    (expr.symbol (symbol def))
    (car expr)
    (expr.list
      (cons 
        (expr.symbol (symbol fn))
        (cdr expr)))))

;; Creates an external definition with a given name.
(macrofn extern expr
  (apply-vararg expr.list
    (expr.symbol (symbol def))
    (car expr)
    (car expr)))