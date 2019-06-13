;;; Macro.m

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
