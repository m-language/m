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
(def and
  (fn x y
    (if x (force y) false)))

;; True if either argument is true.
(def or
  (fn x y
    (if x true (force y))))

;; True if its argument is false.
(def not
  (fn x
    (if x false true)))

;; Macro for and which delays the second argument.
(macro &
  (fn expr
    (list3
      (left (symbol and))
      (car expr)
      (right (cons (left (symbol delay)) (list1 (cadr expr)))))))

;; Macro for or which delays the second argument.
(macro |
  (fn expr
    (list3
      (left (symbol or))
      (car expr)
      (right (cons (left (symbol delay)) (list1 (cadr expr)))))))

;; Macro for control flow.
(macro if
  (fn expr
    (list2
      (left (symbol force))
      (right
        (list3
          (car expr)
          (right (cons (left (symbol delay)) (list1 (cadr expr))))
          (right (cons (left (symbol delay)) (list1 (caddr expr)))))))))