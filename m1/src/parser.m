;; The escape code of a character.
(def escape
  (fn char
    (if (ap char.= char letter-b) backspace
    (if (ap char.= char letter-t) tab
    (if (ap char.= char letter-n) linefeed
    (if (ap char.= char letter-v) vtab
    (if (ap char.= char letter-f) formfeed
    (if (ap char.= char letter-r) carriage-return
      char))))))))

;; True if a character an identifier separator.
(def separator?
  (fn char
    (ap or (ap whitespace? char)
        (fn ""
          (ap or (ap char.= char open-parentheses)
              (fn ""
                (ap or (ap char.= char close-parentheses)
                    (fn ""
                      (ap or (ap char.= char semicolon)
                          (fn ""
                            (ap char.= char quote)))))))))))

;; A position in a file.
(def position
  (ap new-data (symbol position)
    (ap list2 (symbol line) (symbol char))))

(def position.line (ap field (symbol position) (symbol line)))
(def position.char (ap field (symbol position) (symbol char)))

(def start-position (ap position nat.1 nat.1))

;; The position of the next char.
(def next-char
  (fn p
    (ap position
      (ap position.line p)
      (ap nat.+ nat.1 (ap position.char p)))))

;; The position of the next line.
(def next-line
  (fn p
    (ap position
      (ap nat.+ nat.1 (ap position.line p))
      nat.1)))

;; The result of parsing an expression.
(def parse-result
  (ap new-data (symbol parse-result)
    (ap list2 (symbol rest) (symbol expr))))

(def parse-result.rest (ap field (symbol parse-result) (symbol rest)))
(def parse-result.expr (ap field (symbol parse-result) (symbol expr)))

;; Parses an M commment.
(def parse-comment
  (fn input
    (fn path
      (fn position
        (if (ap or (ap nil? input)
                (fn "" (ap newline? (ap car input))))
          (ap parse-expr input path position)
          (ap parse-comment (ap cdr input) path position))))))

;; Parses an M identifier literal expression given an input.
(def parse-identifier-literal-expr
  (fn input
    (fn path
      (fn start
        (fn end
          (fn acc
            (ap with (ap car input)
              (fn head
                (if (ap char.= head quote)
                  (ap parse-result
                    (ap cdr input)
                    (ap identifier-expr (ap reverse acc) path start end))
                (if (ap char.= head backslash)
                  (ap parse-identifier-literal-expr
                    (ap cddr input)
                    path
                    start
                    (ap next-char (ap next-char end))
                    (ap cons (ap escape (ap cadr input)) acc))
                (if (ap newline? head)
                  (ap parse-identifier-literal-expr
                    (ap cdr input)
                    path
                    start
                    (ap next-line end)
                    (ap cons head acc))
                  (ap parse-identifier-literal-expr
                    (ap cdr input)
                    path
                    start
                    (ap next-char end)
                    (ap cons head acc)))))))))))))

;; Parses an M identifier expression given an input.
(def parse-identifier-expr
  (fn input
    (fn path
      (fn start
        (fn end
          (fn acc
            (if (ap separator? (ap car input))
              (ap parse-result input
                (ap identifier-expr (ap reverse acc) path start end))
              (ap parse-identifier-expr
                (ap cdr input)
                path
                start
                (ap next-char end)
                (ap cons (ap car input) acc)))))))))

;; Parses an M list expression given an input.
(def parse-list-expr
  (fn input
    (fn path
      (fn start
        (fn end
          (fn acc
            (if (ap char.= (ap car input) close-parentheses)
              (ap parse-result
                (ap cdr input)
                (ap list-expr (ap reverse acc) path start end))
              (ap with (ap parse-expr input path end)
                (fn result
                  (ap parse-list-expr
                    (ap parse-result.rest result)
                    path
                    start
                    (ap expr.end (ap parse-result.expr result))
                    (ap cons (ap parse-result.expr result) acc)))))))))))

;; Parses an M expression given an input.
(def parse-expr
  (fn input
    (fn path
      (fn position
        (ap with (ap car input)
          (fn head
            (if (ap char.= head open-parentheses)
              (ap parse-list-expr (ap cdr input) path (ap next-char position) (ap next-char position) ())
            (if (ap char.= head quote)
              (ap parse-identifier-literal-expr (ap cdr input) path (ap next-char position) (ap next-char position) ())
            (if (ap char.= head semicolon)
              (ap parse-comment (ap cdr input) path (ap next-char position))
            (if (ap newline? head)
              (ap parse-expr (ap cdr input) path (ap next-line position))
            (if (ap whitespace? head)
              (ap parse-expr (ap cdr input) path (ap next-char position))
              (ap parse-identifier-expr input path position position ()))))))))))))

;; Parses an M program given an input.
(def parse
  (fn input
    (fn path
      (fn position
        (fn acc
          (if (ap nil? input)
            (ap reverse acc)
            (ap with (ap parse-expr input path position)
              (fn result
                (ap parse
                  (ap parse-result.rest result)
                  path
                  (ap expr.end (ap parse-result.expr result))
                  (ap cons (ap parse-result.expr result) acc))))))))))

;; Parses an M program given a file.
(def parse-file
  (fn file
    (fn path
      (fn init
        (ap then-run-with (ap file.directory? file)
          (fn directory?
            (if directory?
              (ap then-run-with (ap file.child-files file)
                (fn child-files
                  (ap fold child-files (ap return ())
                    (fn acc
                      (fn child
                        (ap then-run-with
                          (ap parse-file
                            child
                            (if init
                              ()
                              (ap concat path (ap append (ap file.name file) dot)))
                            false)
                          (fn parse
                            (ap run-with acc
                              (fn exprs
                                (ap concat exprs parse))))))))))
              (ap run-with (ap file.read file)
                (fn chars
                  (ap parse
                    chars
                    (ap concat path (ap file.name-without-extension file))
                    (ap position nat.1 nat.1)
                    ()))))))))))