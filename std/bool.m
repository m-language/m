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
(macrofn & exprs
  (apply-vararg list-expr0
    (symbol-expr0 (symbol and))
    (car exprs)
    (list-expr0 (cons (symbol-expr0 (symbol delay)) (list (cadr exprs))))))

;; Macro for or which delays the second argument.
(macrofn | exprs
  (apply-vararg list-expr0
    (symbol-expr0 (symbol or))
    (car exprs)
    (list-expr0 (cons (symbol-expr0 (symbol delay)) (list (cadr exprs))))))

;; Macro for control flow.
(macro if
  (fn exprs
    (apply-vararg list-expr0
      (symbol-expr0 (symbol force))
      (apply-vararg list-expr0
        (car exprs)
        (list-expr0 (cons (symbol-expr0 (symbol delay)) (list (cadr exprs))))
        (list-expr0 (cons (symbol-expr0 (symbol delay)) (list (caddr exprs))))))))