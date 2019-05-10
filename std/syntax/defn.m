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
          (cons 
            (expr/symbol (symbol fn))
            (cdr exprs)))))))

;; Useful macro for doing recursive macro calls,
;; (macro-call-expr <macro-name> <list of arguments>)
(macrofn macro-call-expr env exprs
  (if (nil? exprs)
    (result/error (symbol "No exprs passed to macro-call-expr"))
    (result/success
      (apply-vararg expr/list
        (expr/symbol (symbol expr/list))
        (apply-vararg expr/list
          (expr/symbol (symbol cons))
          (apply-vararg expr/list
            (expr/symbol (symbol expr/symbol))
            (expr/list (list (expr/symbol (symbol symbol)) (car exprs))))
          (cadr exprs))))))


;; Macro for defining functions.
(macrofn defn env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol def))
      (car exprs)
      (expr/list
        (cons 
          (expr/symbol (symbol fn))
          (cdr exprs))))))
