;; True if a character an symbol separator.
(defn separator? char
  (| (whitespace? char)
  (| (char.= char open-parentheses)
  (| (char.= char close-parentheses)
  (| (char.= char semicolon)
     (char.= char quote))))))

;; Parses an M comment.
(defn parse-comment path input position continue
  (if (| (nil? input) (newline? (car input)))
    (continue path input position)
    (parse-comment path (cdr input) (next-char position) continue)))

;; Parses unused characters.
(defn parse-unused path input position continue
  (if (nil? input)
    (continue path input position)
    (let head (car input)
      (if (whitespace? head)
        (parse-unused path (cdr input) (next-char position) continue)
      (if (char.= head linefeed)
        (parse-unused path (cdr input) (next-line position) continue)
      (if (char.= head semicolon)
        (parse-comment path (cdr input) (next-char position)
          (fn path input position
            (parse-unused path input position continue)))
        (continue path input position)))))))

;; Parses an M single quote.
(defn parse-single-quote path input position continue
  (if (nil? input)
    (error (symbol "Unexpected end of file"))
    (let head (car input)
      (if (char.= head quote)
        (continue () path (cdr input) (next-char position))
        (parse-single-quote path (cdr input) ((if (newline? head) next-line next-char) position)
          (fn chars path input position
            (continue (cons head chars) path input position)))))))

;; Parses an M double quote.
(defn parse-double-quote path input position continue
  (if (nil? input)
    (error (symbol "Unexpected end of file"))
    (let head (car input)
      (if (& (char.= head quote) (char.= (cadr input) quote))
        (continue () path (cddr input) (next-char (next-char position)))
        (parse-double-quote path (cdr input) ((if (newline? head) next-line next-char) position)
          (fn chars path input position
            (continue (cons head chars) path input position)))))))

;; Parses an M symbol literal.
(defn parse-symbol-literal path input position continue
  (if (nil? input)
    (error (symbol "Unexpected end of file"))
    (if (char.= (car input) quote)
      (parse-double-quote path (cdr input) (next-char position) continue)
      (parse-single-quote path input position continue))))

;; Parses an M symbol
(defn parse-symbol path input position continue
  (let head (car input)
    (if (| (nil? input) (separator? head))
      (continue () path input position)
      (parse-symbol path (cdr input) (next-char position)
        (fn chars path input position
          (continue (cons head chars) path input position))))))

;; Parses an M list.
(defn parse-list parse-expr path input position continue
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
(defn parse-expr path input position continue
  (let head (car input)
    (if (char.= head open-parentheses)
      (parse-list parse-expr path (cdr input) (next-char position)
        (fn exprs path input position'
          (continue (list-expr exprs path position position') path input position')))
    (if (char.= head quote)
      (parse-symbol-literal path (cdr input) (next-char position)
        (fn chars path input position'
          (continue (symbol-expr chars path position position') path input position')))
      (parse-symbol path input position
        (fn chars path input position'
          (continue (symbol-expr chars path position position') path input position')))))))

;; Parses an M program.
(defn parse path input position
  (parse-unused path input position
    (fn path input position
      (if (nil? input)
        ()
        (parse-expr path input position
          (fn expr path input position
            (cons expr (parse path input position))))))))

;; Parses an M program given a file.
(defn parse-file file
  (parse-file' file () true))

(defn parse-file' file path init
  (do directory? (file.directory? file)
    (if directory?
      (do child-files (file.child-files file)
        (fold child-files (impure ())
          (fn acc child
            (do parse
                  (parse-file' child
                    (if init () (concat path (append (file.name file) slash)))
                    false)
                exprs acc
              (impure (concat exprs parse))))))
      (do chars (file.read file)
        (impure
          (parse
            (concat path (file.name-without-extension file))
            chars
            (position nat.1 nat.1)))))))