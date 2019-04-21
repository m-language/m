;;; Macro.m
;;;
;;; Various generic macros.

;; Macro for defining macro functions.
(macro macrofn
  (fn exprs
    (apply-vararg macro/list
      (macro/symbol (symbol macro))
      (car exprs)
      (macro/list
        (cons 
          (macro/symbol (symbol fn))
          (cdr exprs))))))

;; Macro for defining functions.
(macrofn defn exprs
  (apply-vararg macro/list
    (macro/symbol (symbol def))
    (car exprs)
    (macro/list
      (cons 
        (macro/symbol (symbol fn))
        (cdr exprs)))))

;; Macro for defining local variables.
(macrofn let exprs
  (if (nil? (cdr exprs)) (macro/list exprs)
    (apply-vararg macro/list
      (macro/symbol (symbol with))
      (cadr exprs)
      (apply-vararg macro/list
        (macro/symbol (symbol fn))
        (car exprs)
        (macro/list
          (cons 
            (macro/symbol (symbol let))
            (cddr exprs)))))))

;; Creates an external definition with a given name.
(macrofn extern exprs
  (apply-vararg macro/list
    (macro/symbol (symbol def))
    (car exprs)
    (car exprs)))