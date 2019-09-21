(defn m-backend' in out trees
  (let desugared (desugar-trees trees)
    (file.write out desugared)))

(defn desugar-trees trees
  (flat-map trees desugar-tree))

(defnrec desugar-tree tree
  (pcond (symbol.= (type-name tree))
    (symbol tree/val)
      (desugar-quote (tree/val.name tree))
    (symbol tree/def)
      (concat (symbol "(def ")
        (concat (desugar-quote (tree/def.name tree))
          (concat (symbol " ")
            (concat (desugar-tree (tree/def.value tree))
              (symbol ")")))))
    (symbol tree/fn)
      (concat (symbol "(fn ")
        (concat (desugar-quote (tree/fn.arg tree))
          (concat (symbol " ")
            (concat (desugar-tree (tree/fn.value tree))
              (symbol ")")))))
    (symbol tree/ap)
      (concat (symbol "(")
        (concat (desugar-tree (tree/ap.fn tree))
          (concat (symbol " ")
            (concat (desugar-tree (tree/ap.arg tree))
              (symbol ")")))))
    (symbol tree/symbol)
      (concat (symbol "(symbol ")
        (concat (desugar-quote (tree/symbol.name tree))
          (symbol ")")))
    (error (symbol "..."))))

;; Quotes a variable with invalid characters.
(defn desugar-quote name
  (if (| (desugar-should-quote? name) (nil? name))
    (cons quote
      ((swap append) quote
        (desugar-quote' name)))
    name))

(defnrec desugar-quote' name
  (cond
    (nil? name) name
    (char.= quote (car name))
      (cons quote (cons quote (desugar-quote' (cdr name))))
      (cons (car name) (desugar-quote' (cdr name)))))

;; Tests if a name should be quoted.
(defnrec desugar-should-quote? name
  (if (nil? name) false
    (let char (car name)
      (| (char.= char quote)
      (| (char.= char open-parentheses)
      (| (char.= char close-parentheses)
      (| (char.= char semicolon)
      (| (whitespace? char)
         (desugar-should-quote? (cdr name))))))))))
