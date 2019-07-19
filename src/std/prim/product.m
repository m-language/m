;;; Product.m
;;;
;;; An implementation of products as a function which applies a function to each
;;; ov its values.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of products.

;; The singleton nil product, a product with no values.
(def product/nil
  (fn f f))

;; Prepends a value to a product.
(def product/cons
  (fn value product
    (fn f (product (f value)))))

;; Creates a literal product.
(macro product
  (fn env exprs
    (result/success
      (((nil? exprs) (const (expr/symbol (symbol product/nil)))
        (fn _
          (expr/list
            (cons (expr/symbol (symbol product/cons))
            (cons (car exprs)
            (cons (expr/list (cons (expr/symbol (symbol list)) (cdr exprs)))
              nil)))))) nil))))
