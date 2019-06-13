;; True if a character an symbol separator.
(defn separator? char
  (| (whitespace? char)
  (| (char.= char open-parentheses)
  (| (char.= char close-parentheses)
  (| (char.= char semicolon)
     (char.= char quote))))))

;; Parses an M comment.
(defnrec parse-comment path input position continue
  (if (| (nil? input) (newline? (car input)))
    (continue path input position)
    (parse-comment path (cdr input) (next-char position) continue)))

;; Parses unused characters.
(defnrec parse-unused path input position continue
  (if (nil? input)
    (continue path input position)
    (let head (car input)
      (cond
        (char.= head linefeed)
          (parse-unused path (cdr input) (next-line position) continue)
        (whitespace? head)
          (parse-unused path (cdr input) (next-char position) continue)
        (char.= head semicolon)
          (parse-comment path (cdr input) (next-char position)
            (fn path input position
              (parse-unused path input position continue)))
          (continue path input position)))))

;; Parses an M single quote.
(defnrec parse-single-quote path input position continue
  (if (nil? input)
    (error (symbol "Unexpected end of file"))
    (let head (car input)
      (if (char.= head quote)
        (continue () path (cdr input) (next-char position))
        (parse-single-quote path (cdr input) ((if (char.= head linefeed) next-line next-char) position)
          (fn chars path input position
            (continue (cons head chars) path input position)))))))

;; Parses an M double quote.
(defnrec parse-double-quote path input position continue
  (if (nil? input)
    (error (symbol "Unexpected end of file"))
    (let head (car input)
      (if (& (char.= head quote) (char.= (cadr input) quote))
        (continue () path (cddr input) (next-char (next-char position)))
        (parse-double-quote path (cdr input) ((if (char.= head linefeed) next-line next-char) position)
          (fn chars path input position
            (continue (cons head chars) path input position)))))))

;; Parses an M symbol literal.
(defnrec parse-symbol-literal path input position continue
  (if (nil? input)
    (error (symbol "Unexpected end of file"))
    (if (char.= (car input) quote)
      (parse-double-quote path (cdr input) (next-char position) continue)
      (parse-single-quote path input position continue))))

;; Parses an M symbol
(defnrec parse-symbol path input position continue
  (let head (car input)
    (if (| (nil? input) (separator? head))
      (continue () path input position)
      (parse-symbol path (cdr input) (next-char position)
        (fn chars path input position
          (continue (cons head chars) path input position))))))

;; Parses an M list.
(defnrec parse-list parse-expr path input position continue
  (if (nil? input)
    (error (symbol "Unexpected end of file"))
    (if (char.= (car input) close-parentheses)
      (continue () path (cdr input) (next-char position))
      (parse-unused path input position
        (fn path input position
          (parse-expr path input position
            (fn expr path input position
              (parse-list parse-expr path input position
                (fn exprs path input position
                  (continue (cons expr exprs) path input position))))))))))

;; Parses an M expression.
(defnrec parse-expr path input position continue
  (let head (car input)
    (pcond (char.= head)
      open-parentheses
        (parse-list parse-expr path (cdr input) (next-char position)
          (fn exprs path input position'
            (continue (list-expr exprs path position position') path input position')))
      quote
        (parse-symbol-literal path (cdr input) (next-char position)
          (fn chars path input position'
            (continue (symbol-expr chars path position position') path input position')))
      (parse-symbol path input position
        (fn chars path input position'
          (continue (symbol-expr chars path position position') path input position'))))))

;; Parses an M program.
(defnrec parse path input position
  (parse-unused path input position
    (fn path input position
      (if (nil? input)
        ()
        (parse-expr path input position
          (fn expr path input position
            (cons expr (parse path input position))))))))

;; Parses an M program given a file.
(defn parse-file file
  (do tree-map (file->tree-map file)
      directory? (file.directory? file)
    (tree-map.fold tree-map (impure ())
      (fn !acc path file
        (do chars (file.read file)
            acc !acc
          (impure
            (concat acc
              (parse 
                (init (init (cdr (flat-map (if directory? (cdr path) path) (cons slash)))))
                chars
                (position nat.1 nat.1)))))))))
