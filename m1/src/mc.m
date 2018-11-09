(def undefined ())

(def true ())
(def false ())

(def nil ())
(def cons ())
(def car ())
(def cdr ())

(def add-int ())
(def symbol->int ())

(def eq-char ())
(def int->char ())

(def eq-symbol ())

(def type-name ())

(def new-data ())
(def field ())

(def then-run ())
(def run-with ())
(def then-run-with ())
(def run-unsafe ())

(def file ())

(def args ())

(def ~generate ())

(def pair
  (new-data (symbol pair)
    (cons (symbol first)
    (cons (symbol second)
      nil))))

(def new-pair
  (lambda first
    (lambda second
      (pair (cons first (cons second nil))))))

(def first (field (symbol pair) (symbol first)))
(def second (field (symbol pair) (symbol second)))

(def and
  (lambda x
    (lambda y
      (if x (y nil) false))))

(def or
  (lambda x
    (lambda y
      (if x true (y nil)))))

(def not
  (lambda x
    (if x false true)))

(def compose
  (lambda f
    (lambda g
      (lambda x
        (f (g x))))))

(def is-nil
  (lambda x
    (eq-symbol (type-name x) (symbol nil))))

(def cadr (compose car cdr))

(def take-while
  (lambda list
    (lambda f
      (if (is-nil list)
        nil
        (if (f (car list))
          (cons (car list) (take-while (cdr list) f))
          nil)))))

(def parse-failure
  (new-data (symbol parse-failure)
    (cons (symbol state)
      nil)))

(def new-parse-failure
  (lambda state
    (parse-failure (cons state nil))))

(def parse-failure.state (field (symbol parse-failure) (symbol state)))

(def parse-success
  (new-data (symbol parse-success)
    (cons (symbol value)
    (cons (symbol state)
    (cons (symbol rest)
      nil)))))

(def new-parse-success
  (lambda value
    (lambda state
      (lambda rest
        (parse-success (cons value (cons state (cons rest nil))))))))

(def parse-success.value (field (symbol parse-success) (symbol value)))
(def parse-success.state (field (symbol parse-success) (symbol state)))
(def parse-success.rest (field (symbol parse-success) (symbol rest)))

(def is-parse-success
  (lambda x
    (eq-symbol (type-name x) (symbol parse-success))))

(def predicate-parser
  (lambda f
    (lambda input
      (lambda state
        (if (and (not (is-nil input))
                 (lambda unused (f (car input))))
          (new-parse-success (car input) state (cdr input))
          (new-parse-failure state))))))

(def map-parser
  (lambda parser
    (lambda f
      (lambda input
        (lambda state
          (f (parser input state)))))))

(def map-parser-success
  (lambda parser
    (lambda f
      (map-parser parser
        (lambda result
          (if (is-parse-success result)
            (f result)
            result))))))

(def map-parser-value
  (lambda parser
    (lambda f
      (map-parser-success parser
        (lambda success
          (new-parse-success
            (f (parse-success.value success))
            (parse-success.state success)
            (parse-success.rest success)))))))

(def map-parser-state
  (lambda parser
    (lambda f
      (map-parser-success parser
        (lambda success
          (new-parse-success
            (parse-success.value success)
            (f (parse-success.state success))
            (parse-success.rest success)))))))

(def provide-past-state
  (lambda parser
    (lambda input
      (lambda state
        ((map-parser-value parser
          (lambda value
            (new-pair value state)))
        input state)))))

(def combine-parser
  (lambda parser1
    (lambda parser2
      (lambda input
        (lambda state
          ((lambda parser1-result
            (if (is-parse-success parser1-result)
              ((lambda parser2-result
                (if (is-parse-success parser2-result)
                  (new-parse-success
                    (new-pair
                      (parse-success.value parser1-result)
                      (parse-success.value parser2-result))
                    (parse-success.state parser2-result)
                    (parse-success.rest parser2-result))
                  parser2-result))
              (parser2
                (parse-success.rest parser1-result)
                (parse-success.state parser1-result)))
              parser1-result))
          (parser1 input state)))))))

(def combine-parser-left
  (lambda parser1
    (lambda parser2
      (map-parser-value (combine-parser parser1 parser2) first))))

(def combine-parser-right
  (lambda parser1
    (lambda parser2
      (map-parser-value (combine-parser parser1 parser2) second))))

(def repeat-parser
  (lambda parser
    (lambda input
      (lambda state
        ((lambda result
          (if (is-parse-success result)
            ((lambda rest-result
              (new-parse-success
                (cons
                  (parse-success.value result)
                  (parse-success.value rest-result))
                (parse-success.state rest-result)
                (parse-success.rest rest-result)))
            (repeat-parser parser
              (parse-success.rest result)
              (parse-success.state result)))
            (new-parse-success nil state input)))
        (parser input state))))))

(def repeat-parser1
  (lambda parser
    (map-parser-value
      (combine-parser parser (repeat-parser parser))
        (lambda pair
          (cons (first pair) (second pair))))))

(def alternative-parser
  (lambda parser1
    (lambda parser2
      (lambda input
        (lambda state
          ((lambda parser1-result
            (if (is-parse-success parser1-result)
              parser1-result
              (parser2 input state)))
          (parser1 input state)))))))

(def lazy-parser
  (lambda parser
    (lambda input
      (lambda state
        ((parser nil) input state)))))

(def identifier-expr
  (new-data (symbol identifier-expr)
    (cons (symbol name)
    (cons (symbol line)
      nil))))

(def new-identifier-expr
  (lambda name
    (lambda line
      (identifier-expr (cons name (cons line nil))))))

(def identifier-expr.name (field (symbol identifier-expr) (symbol name)))
(def identifier-expr.line (field (symbol identifier-expr) (symbol line)))

(def list-expr
  (new-data (symbol list-expr)
    (cons (symbol exprs)
    (cons (symbol line)
      nil))))

(def new-list-expr
  (lambda exprs
    (lambda line
      (list-expr (cons exprs (cons line nil))))))

(def list-expr.exprs (field (symbol list-expr) (symbol exprs)))
(def list-expr.line (field (symbol list-expr) (symbol line)))

(def one (symbol->int (symbol 1)))

(def symbol->int->char (compose int->char symbol->int))

(def open-parentheses (symbol->int->char (symbol 40)))
(def close-parentheses (symbol->int->char (symbol 41)))
(def semicolon (symbol->int->char (symbol 59)))
(def dot (symbol->int->char (symbol 46)))

(def space (symbol->int->char (symbol 32)))
(def tab (symbol->int->char (symbol 9)))
(def linefeed (symbol->int->char (symbol 10)))
(def vtab (symbol->int->char (symbol 11)))
(def formfeed (symbol->int->char (symbol 12)))
(def carriage-return (symbol->int->char (symbol 13)))

(def is-newline
  (lambda char
    (or (eq-char char linefeed)
        (lambda unused
          (or (eq-char char carriage-return)
              (lambda unused
                (eq-char char formfeed)))))))

(def is-whitespace
  (lambda char
    (or (is-newline char)
        (lambda unused
          (or (eq-char char space)
              (lambda unused
                (or (eq-char char tab)
                    (lambda unused (eq-char char vtab)))))))))

(def is-identifier-character
  (lambda char
    (not
      (or (is-whitespace char)
          (lambda unused
            (or (eq-char char open-parentheses)
                (lambda unused
                  (eq-char char close-parentheses))))))))

(def file.read (field (symbol file) (symbol read)))
(def file.name (field (symbol file) (symbol name)))

(def file.name-without-extension
  (lambda file
    (run-with (file.name file)
      (lambda name
        (take-while name
          (lambda char
            (not (eq-char char dot))))))))

(def char-parser
  (lambda char
    (predicate-parser (eq-char char))))

(def newline-parser
  (map-parser-state
    (predicate-parser is-newline)
    (add-int one)))

(def whitespace-parser
  (alternative-parser
    newline-parser
    (predicate-parser is-whitespace)))

(def comment-parser
  (combine-parser
    (char-parser semicolon)
    (repeat-parser (predicate-parser (compose not is-newline)))))

(def ignore-unused
  (lambda parser
    (combine-parser-right
      (repeat-parser (alternative-parser whitespace-parser comment-parser))
      parser)))

(def identifier-char-parser
  (predicate-parser is-identifier-character))

(def identifier-expr-parser
  (ignore-unused
    (map-parser-value
      (provide-past-state
        (repeat-parser1 identifier-char-parser))
      (lambda pair
        (new-identifier-expr (first pair) (second pair))))))

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
        (new-list-expr (first pair) (second pair))))))

(def expr-parser
  (alternative-parser
    identifier-expr-parser
    list-expr-parser))

(def parser
  (repeat-parser expr-parser))

(def parse
  (lambda input
    (parse-success.value
      (parser input one))))

(def compile
  (lambda in-file
    (lambda out-file
      (then-run-with (file.read in-file)
        (lambda char-stream
          (then-run-with (file.name-without-extension in-file)
            (lambda name
              (~generate name out-file
                (parse char-stream)))))))))

(run-unsafe
  (compile
    (file (car args))
    (file (cadr args))))