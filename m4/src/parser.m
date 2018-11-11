(include predef)
(include parser-combinators)
(include expr)

;; Parses a single character.
(def char-parser
  (lambda char
    (predicate-parser (eq-char char))))

;; Parses a newline character.
(def newline-parser
  (map-parser-state
    (predicate-parser is-newline)
    (add-int one)))

;; Parses a whitespace character.
(def whitespace-parser
  (alternative-parser
    newline-parser
    (predicate-parser is-whitespace)))

;; Parses a comment.
(def comment-parser
  (combine-parser
    (predicate-parser (eq-char semicolon))
    (repeat-parser (predicate-parser (compose not is-newline)))))

;; Wraps [parser] to ignore whitepace and comments.
(def ignore-unused
  (lambda parser
    (combine-parser-right
      (repeat-parser (alternative-parser whitespace-parser comment-parser))
      parser)))

;; Parses a single identifier character.
(def identifier-char-parser
  (predicate-parser is-identifier-character))

;; Parses an escape character in an identifier literal.
(def identifier-literal-escape-parser
  (combine-parser-right (char-parser backslash)
    (map-parser-value success-parser escape-map)))

;; Parses a single identifier literal character.
(def identifier-literal-char-parser
  (predicate-parser (compose not (eq-char quote))))

;; Parses an identifier literal.
(def identifier-literal-parser
  (combine-parser-right
    (char-parser quote)
    (combine-parser-left
      (repeat-parser
        (alternative-parser
          identifier-literal-escape-parser
          identifier-literal-char-parser))
      (char-parser quote))))

;; Parses an identifier expression.
(def identifier-expr-parser
  (ignore-unused
    (map-parser-value
      (provide-past-state
        (alternative-parser
          identifier-literal-parser
          (repeat-parser1 identifier-char-parser)))
      (lambda pair
        (identifier-expr (first pair) (second pair))))))

;; Parses a list expression.
(def list-expr-parser
  (ignore-unused
    (map-parser-value
      (provide-past-state
        (combine-parser-right
          (char-parser open-parentheses)
          (combine-parser-left
            (lazy-parser (lambda unused parser))
            (char-parser close-parentheses))))
      (lambda pair
        (list-expr (first pair) (second pair))))))

;; Parses an M expression.
(def expr-parser
  (alternative-parser
    identifier-expr-parser
    list-expr-parser))

;; Parses an M program.
(def parser
  (repeat-parser expr-parser))

;; The result of parsing an M program.
(def parse
  (lambda input
    (parse-success.value
      (parser input one))))