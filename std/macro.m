;;; Macro.m
;;;
;;; Various generic macros.

(def expr.symbol left)
(def expr.list right)

;; Macro for defining macro functions.
(macro macrofn
  (fn exprs
    (apply-vararg expr.list
      (expr.symbol (symbol macro))
      (car exprs)
      (expr.list
        (cons 
          (expr.symbol (symbol fn))
          (cdr exprs))))))

;; Macro for defining functions.
(macrofn defn exprs
  (apply-vararg expr.list
    (expr.symbol (symbol def))
    (car exprs)
    (expr.list
      (cons 
        (expr.symbol (symbol fn))
        (cdr exprs)))))

;; Macro for defining local variables.
(macrofn let exprs
  (if (nil? (cdr exprs)) (expr.list exprs)
    (apply-vararg expr.list
      (expr.symbol (symbol with))
      (cadr exprs)
      (apply-vararg expr.list
        (expr.symbol (symbol fn))
        (car exprs)
        (expr.list
          (cons 
            (expr.symbol (symbol let))
            (cddr exprs)))))))

;; Creates an external definition with a given name.
(macrofn extern exprs
  (apply-vararg expr.list
    (expr.symbol (symbol def))
    (car exprs)
    (car exprs)))