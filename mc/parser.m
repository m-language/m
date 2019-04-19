;; True if a character an symbol separator.
(defn separator? char
  (| (whitespace? char)
  (| (char.= char open-parentheses)
  (| (char.= char close-parentheses)
  (| (char.= char semicolon)
    (char.= char quote))))))

;; A position in a file.
(def position
  (new-data (symbol position)
    (list (symbol line) (symbol char))))

(def position.line (field (symbol position) (symbol line)))
(def position.char (field (symbol position) (symbol char)))

(def start-position (position nat.1 nat.1))

;; The position of the next char.
(defn next-char p
  (position
    (position.line p)
    (nat.+ nat.1 (position.char p))))

;; The position of the next line.
(defn next-line p
  (position
    (nat.+ nat.1 (position.line p))
    nat.1))

;; The result of parsing an expression.
(def parse-result
  (new-data (symbol parse-result)
    (list (symbol rest) (symbol expr))))

(def parse-result.rest (field (symbol parse-result) (symbol rest)))
(def parse-result.expr (field (symbol parse-result) (symbol expr)))

;; Parses an M commment.
(defn parse-comment parse-expr input path position
  (if (| (nil? input) (newline? (car input)))
    (parse-expr input path position)
    (parse-comment parse-expr (cdr input) path position)))

;; Parses a single quote symbol literal expr.
(defn parse-single-quote input path start end acc
  (let head (car input)
    (if (char.= head quote)
      (parse-result
        (cdr input)
        (symbol-expr (reverse acc) path start (next-char end)))
    (if (newline? head)
      (parse-single-quote (cdr input) path start (next-line end) (cons head acc))
      (parse-single-quote (cdr input) path start (next-char end) (cons head acc))))))

;; Parses a double quote symbol literal expr.
(defn parse-double-quote input path start end acc
  (let head (car input)
    (if (char.= head quote)
      (if (char.= (cadr input) quote)
        (parse-result
          (cddr input)
          (symbol-expr (reverse acc) path start (next-char (next-char end))))
        (parse-double-quote (cdr input) path start (next-char end) (cons quote acc)))
    (if (newline? head)
      (parse-double-quote (cdr input) path start (next-line end) (cons head acc))
      (parse-double-quote (cdr input) path start (next-char end) (cons head acc))))))

;; Parses an M symbol literal expression given an input.
(defn parse-symbol-literal-expr input path start end acc
  (if (char.= (car input) quote)
    (parse-double-quote (cdr input) path start (next-char end) acc)
    (parse-single-quote input path start end acc)))

;; Parses an M symbol expression given an input.
(defn parse-symbol-expr input path start end acc
  (if (separator? (car input))
    (parse-result input
      (symbol-expr (reverse acc) path start end))
    (parse-symbol-expr (cdr input) path start (next-char end) (cons (car input) acc))))

;; Parses an M list expression given an input.
(defn parse-list-expr parse-expr input path start end acc
  (if (char.= (car input) close-parentheses)
    (parse-result
      (cdr input)
      (list-expr (reverse acc) path start end))
    (let result (parse-expr input path end)
      (parse-list-expr parse-expr
        (parse-result.rest result)
        path
        start
        (expr.end (parse-result.expr result))
        (cons (parse-result.expr result) acc)))))

;; Parses an M expression given an input.
(defn parse-expr input path position
  (let head (car input)
    (if (char.= head open-parentheses)
      (parse-list-expr parse-expr (cdr input) path (next-char position) (next-char position) ())
    (if (char.= head quote)
      (parse-symbol-literal-expr (cdr input) path (next-char position) (next-char position) ())
    (if (char.= head semicolon)
      (parse-comment parse-expr (cdr input) path (next-char position))
    (if (char.= head linefeed)
      (parse-expr (cdr input) path (next-line position))
    (if (whitespace? head)
      (parse-expr (cdr input) path (next-char position))
      (parse-symbol-expr input path position position ()))))))))

;; Parses an M program given an input.
(defn parse input path position acc
  (if (nil? input)
    (reverse acc)
  (if (char.= linefeed (car input))
    (parse (cdr input) path (next-line position) acc)
  (if (whitespace? (car input))
    (parse (cdr input) path (next-char position) acc)
    (let result (parse-expr input path position)
      (parse
        (parse-result.rest result)
        path
        (expr.end (parse-result.expr result))
        (cons (parse-result.expr result) acc)))))))

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
          (parse chars
            (concat path (file.name-without-extension file))
            (position nat.1 nat.1)
            ()))))))