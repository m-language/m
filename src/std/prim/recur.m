;;; Recur.m
;;;
;;; An implementations of simple recursion using fixed point combinators.
;;;
;;; All definitions in this file are optimized using the backend's native
;;; implementation of recursion.

;; The simplest fixed point combinator which applies a function to itself 
;; lazily and recursively.
(def fix
  (fn f
    ((fn x (f (fn v (x x v))))
     (fn x (f (fn v (x x v)))))))

;; Macro for defining recursive values.
(macrofn defrec env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol def))
      (car exprs)
      (apply-vararg expr/list
        (expr/symbol (symbol fix))
        (cadr exprs)))))

;; Macro for defining recursive functions.
(macrofn defnrec env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol defrec))
      (car exprs)
      (expr/list
        (cons (expr/symbol (symbol fn))
          exprs)))))
