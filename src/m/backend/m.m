(defn m-backend' in out trees
  (file.write out (flat-map trees m/desugar-tree)))

(defnrec m/desugar-tree tree
  (pcond (symbol.= (type-name tree))
    (symbol tree/val)
      (m/desugar-quote (tree/val.name tree))
    (symbol tree/def)
      (concat (symbol "(def ")
        (concat (m/desugar-quote (tree/def.name tree))
          (concat (symbol " ")
            (concat (m/desugar-tree (tree/def.value tree))
              (symbol ")")))))
    (symbol tree/fn)
      (concat (symbol "(fn ")
        (concat (m/desugar-quote (tree/fn.arg tree))
          (concat (symbol " ")
            (concat (m/desugar-tree (tree/fn.value tree))
              (symbol ")")))))
    (symbol tree/ap)
      (concat (symbol "(")
        (concat (m/desugar-tree (tree/ap.fn tree))
          (concat (symbol " ")
            (concat (m/desugar-tree (tree/ap.arg tree))
              (symbol ")")))))
    (symbol tree/symbol)
      (concat (symbol "(symbol ")
        (concat (m/desugar-quote (tree/symbol.name tree))
          (symbol ")")))
    (error (symbol "..."))))

;; Quotes a variable with invalid characters.
(defn m/desugar-quote name
  (if (| (m/desugar-should-quote? name) (nil? name))
    (cons quote
      ((swap append) quote
        (m/desugar-quote' name)))
    name))

(defnrec m/desugar-quote' name
  (cond
    (nil? name) name
    (char.= quote (car name))
      (cons quote (cons quote (m/desugar-quote' (cdr name))))
      (cons (car name) (m/desugar-quote' (cdr name)))))

;; Tests if a name should be quoted.
(defnrec m/desugar-should-quote? name
  (if (nil? name) false
    (let char (car name)
      (| (char.= char quote)
      (| (char.= char open-parentheses)
      (| (char.= char close-parentheses)
      (| (char.= char semicolon)
      (| (whitespace? char)
         (m/desugar-should-quote? (cdr name))))))))))
