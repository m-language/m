;;; Defn.m
;;;
;;; Macros for creating function definitions.

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