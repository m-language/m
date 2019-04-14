;;; Lazy.m

;; Macro which delays the evaluation of an expression.
(macro delay
  (fn expr
    (expr.list
      (cons (expr.symbol (symbol fn))
      (cons (expr.symbol (symbol _))
        expr)))))

;; Macro which forces the delayed evaluation of an expression.
(macro force
  (fn expr
    (apply-vararg expr.list (car expr) (right ()))))

;; Lazy tests.
(def lazy:test
  (apply-vararg combine-tests
    (assert     (force (delay true)))
    (assert-not (force (delay false)))))