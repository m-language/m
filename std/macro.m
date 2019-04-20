;;; Macro.m
;;;
;;; Various generic macros.

;; Macro for defining macro functions.
(macro macrofn
  (fn exprs
    (apply-vararg list-expr0
      (symbol-expr0 (symbol macro))
      (car exprs)
      (list-expr0
        (cons 
          (symbol-expr0 (symbol fn))
          (cdr exprs))))))

;; Macro for defining functions.
(macrofn defn exprs
  (apply-vararg list-expr0
    (symbol-expr0 (symbol def))
    (car exprs)
    (list-expr0
      (cons 
        (symbol-expr0 (symbol fn))
        (cdr exprs)))))

;; Macro for defining local variables.
(macrofn let exprs
  (if (nil? (cdr exprs)) (list-expr0 exprs)
    (apply-vararg list-expr0
      (symbol-expr0 (symbol with))
      (cadr exprs)
      (apply-vararg list-expr0
        (symbol-expr0 (symbol fn))
        (car exprs)
        (list-expr0
          (cons 
            (symbol-expr0 (symbol let))
            (cddr exprs)))))))

;; Creates an external definition with a given name.
(macrofn extern exprs
  (apply-vararg list-expr0
    (symbol-expr0 (symbol def))
    (car exprs)
    (car exprs)))