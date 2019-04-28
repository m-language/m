;;; Bool.m
;;;
;;; An implementation of booleans which are encoded using two argument functions
;;; which ignore one argument.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of booleans.

;; The singleton truthy value, a function which ignores its second argument.
(def true (fn x _ x))

;; The singleton falsy value, a function which ignores its first argument.
(def false (fn _ x x))

;; True if both arguments are true.
(defn and x y
  (if x (force y) false))

;; True if either argument is true.
(defn or x y
  (if x true (force y)))

;; True if its argument is false.
(defn not x
  (if x false true))

;; Macro for and which delays the second argument.
(macrofn & env exprs
  (apply-vararg expr/list
    (expr/symbol (symbol and))
    (car exprs)
    (expr/list (cons (expr/symbol (symbol delay)) (list (cadr exprs))))))

;; Macro for or which delays the second argument.
(macrofn | env exprs
  (apply-vararg expr/list
    (expr/symbol (symbol or))
    (car exprs)
    (expr/list (cons (expr/symbol (symbol delay)) (list (cadr exprs))))))

;; Macro for control flow.
(macro if
  (fn env exprs
    (apply-vararg expr/list
      (expr/symbol (symbol force))
      (apply-vararg expr/list
        (car exprs)
        (expr/list (cons (expr/symbol (symbol delay)) (list (cadr exprs))))
        (expr/list (cons (expr/symbol (symbol delay)) (list (caddr exprs))))))))