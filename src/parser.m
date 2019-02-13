;; The escape code of a character.
(def escape
  (fn char
    (if (char.= char letter-b) backspace
    (if (char.= char letter-t) tab
    (if (char.= char letter-n) linefeed
    (if (char.= char letter-v) vtab
    (if (char.= char letter-f) formfeed
    (if (char.= char letter-r) carriage-return
      char))))))))

;; True if a character an identifier separator.
(def separator?
  (fn char
    (or (whitespace? char)
        (fn ""
          (or (char.= char open-parentheses)
              (fn ""
                (or (char.= char close-parentheses)
                    (fn ""
                      (or (char.= char semicolon)
                          (fn ""
                            (char.= char quote)))))))))))

;; A position in a file.
(def position
  (new-data (symbol position)
    (list2 (symbol line) (symbol char))))

(def position.line (field (symbol position) (symbol line)))
(def position.char (field (symbol position) (symbol char)))

(def start-position (position nat.1 nat.1))

;; The position of the next char.
(def next-char
  (fn p
    (position
      (position.line p)
      (nat.+ nat.1 (position.char p)))))

;; The position of the next line.
(def next-line
  (fn p
    (position
      (nat.+ nat.1 (position.line p))
      nat.1)))

;; The result of parsing an expression.
(def parse-result
  (new-data (symbol parse-result)
    (list2 (symbol rest) (symbol expr))))

(def parse-result.rest (field (symbol parse-result) (symbol rest)))
(def parse-result.expr (field (symbol parse-result) (symbol expr)))

;; Parses an M commment.
(def parse-comment
  (fn input
    (fn path
      (fn position
        (if (or (nil? input)
                (fn "" (newline? (car input))))
          (parse-expr input path position)
          (parse-comment (cdr input) path position))))))

;; Parses an M identifier literal expression given an input.
(def parse-identifier-literal-expr
  (fn input
    (fn path
      (fn start
        (fn end
          (fn acc
            (with (car input)
              (fn head
                (if (char.= head quote)
                  (parse-result
                    (cdr input)
                    (identifier-expr (reverse acc) path start end))
                (if (char.= head backslash)
                  (parse-identifier-literal-expr
                    (cddr input)
                    path
                    start
                    (next-char (next-char end))
                    (cons (escape (cadr input)) acc))
                (if (newline? head)
                  (parse-identifier-literal-expr
                    (cdr input)
                    path
                    start
                    (next-line end)
                    (cons head acc))
                  (parse-identifier-literal-expr
                    (cdr input)
                    path
                    start
                    (next-char end)
                    (cons head acc)))))))))))))

;; Parses an M identifier expression given an input.
(def parse-identifier-expr
  (fn input
    (fn path
      (fn start
        (fn end
          (fn acc
            (if (separator? (car input))
              (parse-result input
                (identifier-expr (reverse acc) path start end))
              (parse-identifier-expr
                (cdr input)
                path
                start
                (next-char end)
                (cons (car input) acc)))))))))

;; Parses an M list expression given an input.
(def parse-list-expr
  (fn input
    (fn path
      (fn start
        (fn end
          (fn acc
            (if (char.= (car input) close-parentheses)
              (parse-result
                (cdr input)
                (list-expr (reverse acc) path start end))
              (with (parse-expr input path end)
                (fn result
                  (parse-list-expr
                    (parse-result.rest result)
                    path
                    start
                    (expr.end (parse-result.expr result))
                    (cons (parse-result.expr result) acc)))))))))))

;; Parses an M expression given an input.
(def parse-expr
  (fn input
    (fn path
      (fn position
        (with (car input)
          (fn head
            (if (char.= head open-parentheses)
              (parse-list-expr (cdr input) path (next-char position) (next-char position) ())
            (if (char.= head quote)
              (parse-identifier-literal-expr (cdr input) path (next-char position) (next-char position) ())
            (if (char.= head semicolon)
              (parse-comment (cdr input) path (next-char position))
            (if (newline? head)
              (parse-expr (cdr input) path (next-line position))
            (if (whitespace? head)
              (parse-expr (cdr input) path (next-char position))
              (parse-identifier-expr input path position position ()))))))))))))

;; Parses an M program given an input.
(def parse
  (fn input
    (fn path
      (fn position
        (fn acc
          (if (nil? input)
            (reverse acc)
          (if (newline? (car input))
            (parse (cdr input) path (next-line position) acc)
          (if (whitespace? (car input))
            (parse (cdr input) path (next-char position) acc)
            (with (parse-expr input path position)
              (fn result
                (parse
                  (parse-result.rest result)
                  path
                  (expr.end (parse-result.expr result))
                  (cons (parse-result.expr result) acc))))))))))))

;; Parses an M program given a file.
(def parse-file
  (fn file
    (fn path
      (fn init
        (then-run-with (file.directory? file)
          (fn directory?
            (if directory?
              (then-run-with (file.child-files file)
                (fn child-files
                  (fold child-files (impure ())
                    (fn acc
                      (fn child
                        (then-run-with
                          (parse-file
                            child
                            (if init
                              ()
                              (concat path (append (file.name file) slash)))
                            false)
                          (fn parse
                            (run-with acc
                              (fn exprs
                                (concat exprs parse))))))))))
              (run-with (file.read file)
                (fn chars
                  (parse
                    chars
                    (concat path (file.name-without-extension file))
                    (position nat.1 nat.1)
                    ()))))))))))