;; Creates a block of expressions, returning the result of the first one.
(macrofn block env exprs
  (if (nil? exprs) (result/error (symbol "Could not match [expr] => (block expr+)"))
  (if (nil? (cdr exprs)) (result/success (car exprs))
    (result/success
      (quote
        (const (unquote (car exprs))
          (block (splice (cdr exprs)))))))))
