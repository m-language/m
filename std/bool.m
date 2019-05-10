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
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol and))
      (car exprs)
      (expr/list (cons (expr/symbol (symbol delay)) (list (cadr exprs)))))))

;; Macro for or which delays the second argument.
(macrofn | env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol or))
      (car exprs)
      (expr/list (cons (expr/symbol (symbol delay)) (list (cadr exprs)))))))

;; Macro for control flow.
(macro if
  (fn env exprs
    (result/success
      (apply-vararg expr/list
        (expr/symbol (symbol force))
        (apply-vararg expr/list
          (car exprs)
          (expr/list (cons (expr/symbol (symbol delay)) (list (cadr exprs))))
          (expr/list (cons (expr/symbol (symbol delay)) (list (caddr exprs)))))))))

;; Macro for multi-branch if expressions, i.e
;; (cond
;;   condition1 value1
;;   condition2 value2
;;   ...
;;   conditionN valueN
;;   else-value)
(macrofn cond env exprs
  (if (nil? exprs) (result/error (symbol "No exprs passed to cond"))
  (if (nil? (cdr exprs))
    ; Base case: we have (cond e), so evaluates to e
    (result/success (car exprs))
    ; Recursive case: (cond c v ...) = (if c v (cond ...))
    (if (nil? (cddr exprs))
      (result/error (symbol "No else case for cond"))
      (result/success
        (apply-vararg expr/list
          (expr/symbol (symbol if))
          (car exprs)
          (cadr exprs)
          (macro-call-expr cond (cddr exprs))))))))

;; Macro to test a predicate against multiple alternatives
;; (cond-satisfy predicate
;;   value1 then-value1
;;   value2 then-value2
;;   ...
;;   valueN then-valueN
;;   else-value)
(macrofn cond-satisfy env exprs
  (if (nil? exprs)
    (result/error (symbol "No exprs passed to cond-satisfy"))
    (let predicate (car exprs) vals (cdr exprs)
      (cond
        (nil? vals) (result/error (symbol "No values after predicate for cond-satisfy"))
        ; Base case, we only have the else-case
        (nil? (cdr vals)) (result/success (car vals))
        ; TODO convert to a call to (cond ...) and do more thorough checks on input
        ; Recursive case, build a chain of if expressions
        (result/success
          (apply-vararg expr/list
            (expr/symbol (symbol if))
            (expr/list (list predicate (car vals)))
            (cadr vals)
            (macro-call-expr cond-satisfy (cons predicate (cddr vals)))))))))
