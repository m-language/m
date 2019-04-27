;;; Defn.m
;;;
;;; Macros for creating function definitions.

;; Macro for defining macro functions.
(macro macrofn
  (fn exprs
    (apply-vararg expr/list
      (expr/symbol (symbol macro))
      (car exprs)
      (expr/list
        (cons 
          (expr/symbol (symbol fn))
          (cdr exprs))))))

;; Macro for defining functions.
(macrofn defn exprs
  (apply-vararg expr/list
    (expr/symbol (symbol def))
    (car exprs)
    (expr/list
      (cons 
        (expr/symbol (symbol fn))
        (cdr exprs)))))