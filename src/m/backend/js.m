(defn js-backend' in out trees
  (file.write out (flat-map trees js/desugar-tree)))

(defnrec js/desugar-tree tree
  (pcond (symbol.= (type-name tree))
    (symbol tree/val)
      (js/desugar-quote (tree/val.name tree))
    (symbol tree/def)
      (let desugared-name (js/desugar-quote (tree/def.name tree))
        (concat (symbol "var ")
          (concat desugared-name
            (concat (symbol " = ")
              (concat (js/desugar-tree (tree/def.value tree))
                (concat (list semicolon linefeed)
                  (concat (symbol "exports.")
                    (concat desugared-name
                      (concat (symbol " = ")
                        (concat desugared-name
                          (list semicolon linefeed)))))))))))
    (symbol tree/fn)
      (concat (symbol "function(")
        (concat (js/desugar-quote (tree/fn.arg tree))
          (concat (symbol ") { return ")
            (concat (js/desugar-tree (tree/fn.value tree))
              (symbol " }")))))
    (symbol tree/ap)
      (concat (js/desugar-tree (tree/ap.fn tree))
        (concat (symbol ".apply(null, [")
          (concat (js/desugar-tree (tree/ap.arg tree))
            (symbol "])"))))
    (symbol tree/symbol)
      (concat (symbol "__symbol(")
        (concat (js/desugar-string (tree/symbol.name tree))
          (symbol ")")))
    (error (symbol "..."))))

(extern normalize)

;; Quotes a variable with invalid characters.
(defn js/desugar-quote name
  (concat (symbol "m_") (normalize name)))

;; Quotes a string.
(defn js/desugar-string name
  (cons double-quote (append (js/desugar-string' name) double-quote)))

(defnrec js/desugar-string' name
  (cond (nil? name) nil
        (char.= double-quote (car name)) (concat (symbol "\""") (js/desugar-string' (cdr name)))
        (char.= backslash (car name)) (concat (symbol "\\") (js/desugar-string' (cdr name)))
        (char.= linefeed (car name)) (concat (symbol "\n") (js/desugar-string' (cdr name)))
        (char.= carriage-return (car name)) (concat (symbol "\r") (js/desugar-string' (cdr name)))
        (char.= tab (car name)) (concat (symbol "\t") (js/desugar-string' (cdr name)))
    (cons (car name) (js/desugar-string' (cdr name)))))
