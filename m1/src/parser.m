;; The escape code of a character.
(def escape
  (lambda char
    (if (char.= char letter-b) backspace
    (if (char.= char letter-t) tab
    (if (char.= char letter-n) linefeed
    (if (char.= char letter-v) vtab
    (if (char.= char letter-f) formfeed
    (if (char.= char letter-r) carriage-return
      char))))))))

;; True if a character an identifier separator.
(def separator?
  (lambda char
    (or (whitespace? char)
        (lambda ""
          (or (char.= char open-parentheses)
              (lambda ""
                (or (char.= char close-parentheses)
                    (lambda ""
                      (or (char.= char semicolon)
                          (lambda ""
                            (char.= char quote)))))))))))

;; A position in a file.
(def position
  (new-data* (symbol position)
    (symbol line) (symbol char) ()))

(def position.line (field (symbol position) (symbol line)))
(def position.char (field (symbol position) (symbol char)))

;; The position of the next char.
(def next-char
  (lambda p
    (position
      (position.line p)
      (nat.+ one (position.char p)))))

;; The position of the next line.
(def next-line
  (lambda p
    (position
      (nat.+ one (position.line p))
      one)))

;; The result of parsing an expression.
(def parse-result
  (new-data* (symbol parse-result)
    (symbol rest) (symbol expr) ()))

(def parse-result.rest (field (symbol parse-result) (symbol rest)))
(def parse-result.expr (field (symbol parse-result) (symbol expr)))

;; Parses an M commment.
(def parse-comment
  (lambda input
    (lambda path
      (lambda position
        (if (newline? (car input))
          (parse-expr input path position)
          (parse-comment (cdr input) path position))))))

;; Parses an M identifier literal expression given [input].
(def parse-identifier-literal-expr
  (lambda input
    (lambda path
      (lambda start
        (lambda end
          (lambda acc
            (with (car input)
              (lambda head
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

;; Parses an M identifier expression given [input].
(def parse-identifier-expr
  (lambda input
    (lambda path
      (lambda start
        (lambda end
          (lambda acc
            (if (separator? (car input))
              (parse-result input (identifier-expr (reverse acc) path start end))
              (parse-identifier-expr
                (cdr input)
                path
                start
                (next-char end)
                (cons (car input) acc)))))))))

;; Parses an M list expression given [input].
(def parse-list-expr
  (lambda input
    (lambda path
      (lambda start
        (lambda end
          (lambda acc
            (if (char.= (car input) close-parentheses)
              (parse-result (cdr input) (list-expr (reverse acc) path start end))
              (with (parse-expr input path end)
                (lambda result
                  (parse-list-expr
                    (parse-result.rest result)
                    path
                    start
                    (expr.end (parse-result.expr result))
                    (cons (parse-result.expr result) acc)))))))))))

;; Parses an M expression given [input].
(def parse-expr
  (lambda input
    (lambda path
      (lambda position
        (with (car input)
          (lambda head
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

;; Parses an M program given [input].
(def parse
  (lambda input
    (lambda path
      (lambda position
        (lambda acc
          (if (nil? input)
            (reverse acc)
            (with (parse-expr input path position)
              (lambda result
                (parse
                  (parse-result.rest result)
                  path
                  (expr.end (parse-result.expr result))
                  (cons (parse-result.expr result) acc))))))))))

;; Parses an M program given a [file].
(def parse-file
  (lambda file
    (lambda path
      (lambda init
        (then-run-with (file.directory? file)
          (lambda directory?
            (if directory?
              (then-run-with (file.child-files file)
                (lambda child-files
                  (fold child-files (function->process (lambda "" ()))
                    (lambda acc
                      (lambda child
                        (then-run-with
                          (parse-file
                            child
                            (if init
                              ()
                              (concat path (append (file.name file) dot)))
                            false)
                          (lambda parse
                            (run-with acc
                              (lambda exprs
                                (concat exprs parse))))))))))
              (run-with (file.read file)
                (lambda chars
                  (parse
                    chars
                    (concat path (file.name-without-extension file))
                    (position one one)
                    ()))))))))))