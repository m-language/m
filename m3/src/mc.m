;;; Predefs

;; The singleton undefined value.
(def undefined ())

;; The singleton truthy value, for which `(if true x y)` evaluates to x.
(def true ())

;; The singleton falsy value, for which `(if false x y)` evaluates to y.
(def false ())

;; The singleton empty list.
(def nil ())

;; Prepends an element to a list.
(def cons ())

;; The first element in a list.
(def car ())

;; The rest of the elements in a list.
(def cdr ())

;; Converts a symbol to a list of characters.
(def symbol->list ())

;; Adds two integers.
(def add-int ())

;; True if the first int is greater than the second int.
(def gt-int ())

;; True if the first int is less than the second int.
(def lt-int ())

;; Converts a symbol to an int.
(def symbol->int ())

;; Converts a character to an int.
(def char->int ())

;; Tests if two characters are equal.
(def eq-char ())

;; Converts an integer to a character.
(def int->char ())

;; Converts a symbol to a character.
(def symbol->char ())

;; Tests if two symbols are equal.
(def eq-symbol ())

;; A symbol representing the type of a value.
(def type-name ())

;; Creates an empty data object given a type.
(def object ())

;; Derives a data object with a field given its name and value.
(def derive ())

;; Accesses a field of data given its name.
(def field ())

;; Combines two processes, running them one after another.
(def then-run ())

;; Runs a function in a process.
(def run-with ())

;; Runs a function which produces a process in a process, then combines them.
(def then-run-with ())

;; Runs a process at top level.
(def run-unsafe ())

;; The root file for this program.
(def local-file ())

;; The list of arguments passed to the program.
(def args ())

;;; Utils

;; A pair of two values.
(def pair
  (lambda first
    (lambda second
      (derive (symbol pair) (symbol first) first
      (derive (symbol pair) (symbol second) second
        (object (symbol pair)))))))

;; The first value in a pair.
(def first (field (symbol pair) (symbol first)))

;; The second value in a pair.
(def second (field (symbol pair) (symbol second)))

;; True if [x] and [y] are true.
(def and
  (lambda x
    (lambda y
      (if x (y nil) false))))

;; True if [x] or [y] is true.
(def or
  (lambda x
    (lambda y
      (if x true (y nil)))))

;; True if [x] is false.
(def not
  (lambda x
    (if x false true)))

;; Composes two functions [f] and [g].
(def compose
  (lambda f
    (lambda g
      (lambda x
        (f (g x))))))

;; Tests if a value is the empty list.
(def is-nil
  (lambda x
    (eq-symbol (type-name x) (symbol nil))))

;; The second element in a list.
(def cadr (compose car cdr))

;; The rest of the rest of the list.
(def cddr (compose cdr cdr))

;; The third element in a list.
(def caddr (compose car cddr))

;; The fourth element in a list.
(def cadddr (compose car (compose cdr cddr)))

;; Appends [elem] to [list].
(def append
  (lambda list
    (lambda elem
      (if (is-nil list)
        (cons elem list)
        (cons (car list) (append (cdr list) elem))))))

;; Maps [list] with function [f].
(def map
  (lambda list
    (lambda f
      (if (is-nil list)
        nil
        (cons (f (car list)) (map (cdr list) f))))))

;; Folds [list] with an accumulator [acc] function [f].
(def fold
  (lambda list
    (lambda acc
      (lambda f
        (if (is-nil list)
          acc
          (fold (cdr list) (f acc (car list)) f))))))

;; Takes elements of [list] while [f] is true.
(def take-while
  (lambda list
    (lambda f
      (if (is-nil list)
        nil
        (if (f (car list))
          (cons (car list) (take-while (cdr list) f))
          nil)))))

;; Tests if [list1] and [list2] are equal given a function [f].
(def eq-list
  (lambda f
    (lambda list1
      (lambda list2
        (if (is-nil list1)
          (is-nil list2)
          (if (is-nil list2)
            false
            (and (f (car list1) (car list2))
                 (lambda unused (eq-list f (cdr list1) (cdr list2))))))))))

;;; Maybe

;; A container for something.
(def some
  (lambda value
    (derive (symbol some) (symbol value) value
      (object (symbol some)))))

;; Tests if a value is a some.
(def is-some
  (lambda x
    (eq-symbol (type-name x) (symbol some))))

(def some.value (field (symbol some) (symbol value)))

;; The lack of something.
(def none (object (symbol none)))

;; Tests if a value is none.
(def is-none
  (lambda x
    (eq-symbol (type-name x) (symbol none))))

;;; Compare

;; Represents that two values are equal.
(def compare= (object (symbol compare=)))

;; Represents that the first value is greater than the second value.
(def compare< (object (symbol compare<)))

;; Represents that the first value is less than the second value.
(def compare> (object (symbol compare>)))

;; Tests if a value is compare=.
(def is-compare=
  (lambda x
    (eq-symbol (type-name x) (symbol compare=))))

;; Tests if a value is compare<.
(def is-compare<
  (lambda x
    (eq-symbol (type-name x) (symbol compare<))))

;; Tests if a value is compare>.
(def is-compare>
  (lambda x
    (eq-symbol (type-name x) (symbol compare>))))

;; Folds over the result of a compare.
(def fold-compare
  (lambda compare
    (lambda <
      (lambda >
        (lambda =
          (if (is-compare< compare)
            (< compare)
            (if (is-compare> compare)
              (> compare)
              (= compare))))))))

;; Compares lists given [compare].
(def compare-list
  (lambda compare
    (lambda list1
      (lambda list2
        (if (and (is-nil list1)
                 (lambda unused (is-nil list2)))
          compare=
        (if (is-nil list1)
          compare<
        (if (is-nil list2)
          compare>
        ((lambda compare-result
          (if (is-compare= compare-result)
            (compare-list compare (cdr list1) (cdr list2))
            compare-result))
          (compare (car list1) (car list2))))))))))

;; Compares ints.
(def compare-int
  (lambda int1
    (lambda int2
      (if (gt-int int1 int2)
        compare>
        (if (lt-int int1 int2)
          compare<
          compare=)))))

;; Compares chars.
(def compare-char
  (lambda char1
    (lambda char2
      (compare-int (char->int char1) (char->int char2)))))

;; Compare strings.
(def compare-string (compare-list compare-char))

;;; Tree Map

;; A node in a tree map.
(def tree-map-node
  (lambda left
    (lambda right
      (lambda key
        (lambda value
          (derive (symbol tree-map-node) (symbol left) left
          (derive (symbol tree-map-node) (symbol right) right
          (derive (symbol tree-map-node) (symbol key) key
          (derive (symbol tree-map-node) (symbol value) value
            (object (symbol tree-map-node)))))))))))

(def tree-map-node.left (field (symbol tree-map-node) (symbol left)))
(def tree-map-node.right (field (symbol tree-map-node) (symbol right)))
(def tree-map-node.key (field (symbol tree-map-node) (symbol key)))
(def tree-map-node.value (field (symbol tree-map-node) (symbol value)))

;; The empty tree map node.
(def tree-map-node-nil (object (symbol tree-map-node-nil)))

;; Tests if a value is tree-map-node-nil.
(def is-tree-map-node-nil
  (lambda x
    (eq-symbol (type-name x) (symbol tree-map-node-nil))))

;; A tree map.
(def tree-map
  (lambda node
    (lambda compare
      (derive (symbol tree-map) (symbol node) node
      (derive (symbol tree-map) (symbol compare) compare
        (object (symbol tree-map)))))))

(def tree-map.node (field (symbol tree-map) (symbol node)))
(def tree-map.compare (field (symbol tree-map) (symbol compare)))

;; Creates a new tree map.
(def tree-map
  (lambda node
    (lambda compare
      (tree-map (cons node (cons compare nil))))))

;; Creates an empty tree map with a given compare.
(def empty-tree-map
  (lambda compare
    (tree-map tree-map-node-nil compare)))

;; Gets the value of a key in a tree map node.
(def tree-map-node.get
  (lambda node
    (lambda compare
      (lambda key
        (if (is-tree-map-node-nil node)
          none
          (fold-compare (compare key (tree-map-node.key node))
            (lambda <
              (tree-map-node.get (tree-map-node.left node) compare key))
            (lambda >
              (tree-map-node.get (tree-map-node.right node) compare key))
            (lambda =
              (some (tree-map-node.value node)))))))))

;; Gets the value of a key in a tree map.
(def tree-map.get
  (lambda map
    (lambda key
      (tree-map-node.get (tree-map.node map) (tree-map.compare map) key))))

;; Puts the value of a key in a tree map node.
(def tree-map-node.put
  (lambda node
    (lambda compare
      (lambda key
        (lambda value
          (if (is-tree-map-node-nil node)
            (tree-map-node tree-map-node-nil tree-map-node-nil key value)
            (fold-compare (compare key (tree-map-node.key node))
              (lambda <
                (tree-map-node
                  (tree-map-node.put
                    (tree-map-node.left node)
                    compare
                    key
                    value)
                  (tree-map-node.right node)
                  (tree-map-node.key node)
                  (tree-map-node.value node)))
              (lambda >
                (tree-map-node
                  (tree-map-node.left node)
                  (tree-map-node.put
                    (tree-map-node.right node)
                    compare
                    key
                    value)
                  (tree-map-node.key node)
                  (tree-map-node.value node)))
              (lambda =
                (tree-map-node
                  (tree-map-node.left node)
                  (tree-map-node.right node)
                  key
                  value)))))))))

;; Puts the value of a key in a tree map.
(def tree-map.put
  (lambda map
    (lambda key
      (lambda value
        (tree-map
          (tree-map-node.put
            (tree-map.node map)
            (tree-map.compare map)
            key
            value)
          (tree-map.compare map))))))

;; Folds [node] with an accumulator [acc] and function [f].
(def tree-map-node.fold
  (lambda node
    (lambda acc
      (lambda f
        (if (is-tree-map-node-nil node)
          acc
          (tree-map-node.fold (tree-map-node.right node)
            (tree-map-node.fold (tree-map-node.left node)
              (f acc (tree-map-node.key node) (tree-map-node.value node))
            f)
          f))))))

;; Folds [map] with an accumulator [acc] and function [f].
(def tree-map.fold
  (lambda map
    (lambda acc
      (lambda f
        (tree-map-node.fold (tree-map.node map) acc f)))))

;; Adds [map1] and [map2].
(def tree-map.add
  (lambda map1
    (lambda map2
      (tree-map.fold map1 map2
        (lambda map
          (lambda key
            (lambda value
              (tree-map.put map key value))))))))

;; Converts a tree map to a list.
(def tree-map->list
  (lambda map
    (tree-map.fold map nil
      (lambda list
        (lambda key
          (lambda value
            (cons (pair key value) list)))))))

;;; Parser Combinators

;; A parse result representing failure.
(def parse-failure
  (lambda state
    (derive (symbol parse-failure) (symbol state) state
      (object (symbol parse-failure)))))

(def parse-failure.state (field (symbol parse-failure) (symbol state)))

;; A parse result representing success.
(def parse-success
  (lambda value
    (lambda state
      (lambda rest
        (derive (symbol parse-success) (symbol value) value
        (derive (symbol parse-success) (symbol state) state
        (derive (symbol parse-success) (symbol rest) rest
          (object (symbol parse-success)))))))))

(def parse-success.value (field (symbol parse-success) (symbol value)))
(def parse-success.state (field (symbol parse-success) (symbol state)))
(def parse-success.rest (field (symbol parse-success) (symbol rest)))

;; Tests if a value is a parse success.
(def is-parse-success
  (lambda x
    (eq-symbol (type-name x) (symbol parse-success))))

;; A parser which succeeds only if [f] of the next element is true.
(def predicate-parser
  (lambda f
    (lambda input
      (lambda state
        (if (and (not (is-nil input))
                 (lambda unused (f (car input))))
          (parse-success (car input) state (cdr input))
          (parse-failure state))))))

;; A parser which always succeeds.
(def success-parser (predicate-parser (lambda unused true)))

;; Maps [parser]'s result with the function [f].
(def map-parser
  (lambda parser
    (lambda f
      (lambda input
        (lambda state
          (f (parser input state)))))))

;; Maps [parser]'s result with the function [f] if the result is a success.
(def map-parser-success
  (lambda parser
    (lambda f
      (map-parser parser
        (lambda result
          (if (is-parse-success result)
            (f result)
            result))))))

;; Maps [parser]'s result's value with the function [f].
(def map-parser-value
  (lambda parser
    (lambda f
      (map-parser-success parser
        (lambda success
          (parse-success
            (f (parse-success.value success))
            (parse-success.state success)
            (parse-success.rest success)))))))

;; Maps [parser]'s result's state with the function [f].
(def map-parser-state
  (lambda parser
    (lambda f
      (map-parser-success parser
        (lambda success
          (parse-success
            (parse-success.value success)
            (f (parse-success.state success))
            (parse-success.rest success)))))))

;; Provides the state before [parser] was run.
(def provide-past-state
  (lambda parser
    (lambda input
      (lambda state
        ((map-parser-value parser
          (lambda value
            (pair value state)))
        input state)))))

;; Combines [parser1] and [parser2].
(def combine-parser
  (lambda parser1
    (lambda parser2
      (lambda input
        (lambda state
          ((lambda parser1-result
            (if (is-parse-success parser1-result)
              ((lambda parser2-result
                (if (is-parse-success parser2-result)
                  (parse-success
                    (pair
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

;; Combines [parser1] and [parser2], deferring to parser1's result.
(def combine-parser-left
  (lambda parser1
    (lambda parser2
      (map-parser-value (combine-parser parser1 parser2) first))))

;; Combines [parser1] and [parser2], deferring to parser2's result.
(def combine-parser-right
  (lambda parser1
    (lambda parser2
      (map-parser-value (combine-parser parser1 parser2) second))))

;; Parses a list of [parser].
(def repeat-parser
  (lambda parser
    (lambda input
      (lambda state
        ((lambda result
          (if (is-parse-success result)
            ((lambda rest-result
              (parse-success
                (cons
                  (parse-success.value result)
                  (parse-success.value rest-result))
                (parse-success.state rest-result)
                (parse-success.rest rest-result)))
            (repeat-parser parser
              (parse-success.rest result)
              (parse-success.state result)))
            (parse-success nil state input)))
        (parser input state))))))

;; Parses a non empty list of [parser].
(def repeat-parser1
  (lambda parser
    (map-parser-value
      (combine-parser parser (repeat-parser parser))
        (lambda pair
          (cons (first pair) (second pair))))))

;; Parses [parser2] if [parser1] fails.
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

;; A parser whose implementation is only evaluated once it is called.
(def lazy-parser
  (lambda parser
    (lambda input
      (lambda state
        ((parser nil) input state)))))

;;; Tree

;; An expression representing an M identifier.
(def identifier-expr
  (lambda name
    (lambda line
      (derive (symbol identifier-expr) (symbol name) name
      (derive (symbol identifier-expr) (symbol line) line
        (object (symbol identifier-expr)))))))

;; True if [x] is an identifier expression.
(def is-identifier-expr
  (lambda x
    (eq-symbol (type-name x) (symbol identifier-expr))))

(def identifier-expr.name (field (symbol identifier-expr) (symbol name)))
(def identifier-expr.line (field (symbol identifier-expr) (symbol line)))

;; An expression representing an M list.
(def list-expr
  (lambda exprs
    (lambda line
      (derive (symbol list-expr) (symbol exprs) exprs
      (derive (symbol list-expr) (symbol line) line
        (object (symbol list-expr)))))))

;; True if [x] is a list expression.
(def is-list-expr
  (lambda x
    (eq-symbol (type-name x) (symbol list-expr))))

(def list-expr.exprs (field (symbol list-expr) (symbol exprs)))
(def list-expr.line (field (symbol list-expr) (symbol line)))

;; The line of an expression.
(def expr.line
  (lambda expr
    (if (is-identifier-expr expr)
      (identifier-expr.line expr)
      (list-expr.line expr))))

;;; Char utils

;; The literal number 0
(def zero (symbol->int (symbol 0)))

;; The literal number 1.
(def one (symbol->int (symbol 1)))

;; The literal character "(".
(def open-parentheses (symbol->char (symbol "(")))

;; The literal character ")".
(def close-parentheses (symbol->char (symbol ")")))

;; The literal character ";".
(def semicolon (symbol->char (symbol ";")))

;; The literal character ".".
(def dot (symbol->char (symbol ".")))

;; The literal character "\"".
(def quote (symbol->char (symbol "\"")))

;; The literal character "\\".
(def backslash (symbol->char (symbol "\\")))

;; The literal character " ".
(def space (symbol->char (symbol " ")))

;; The literal character "\b".
(def backspace (symbol->char (symbol "\b")))

;; The literal character "\t".
(def tab (symbol->char (symbol "\t")))

;; The literal character "\n".
(def linefeed (symbol->char (symbol "\n")))

;; The literal character "\v".
(def vtab (symbol->char (symbol "\v")))

;; The literal character "\f".
(def formfeed (symbol->char (symbol "\f")))

;; The literal character "\r".
(def carriage-return (symbol->char (symbol "\r")))

;; The literal character "b".
(def letter-b (symbol->char (symbol "b")))

;; The literal character "t".
(def letter-t (symbol->char (symbol "t")))

;; The literal character "n".
(def letter-n (symbol->char (symbol "n")))

;; The literal character "v".
(def letter-v (symbol->char (symbol "v")))

;; The literal character "f".
(def letter-f (symbol->char (symbol "f")))

;; The literal character "r".
(def letter-r (symbol->char (symbol "r")))

;; True if a character is "\r", "\n", or "\f".
(def is-newline
  (lambda char
    (or (eq-char char linefeed)
        (lambda unused
          (or (eq-char char carriage-return)
              (lambda unused
                (eq-char char formfeed)))))))

;; True if a character is a newline, " ", "\t", or "\v".
(def is-whitespace
  (lambda char
    (or (is-newline char)
        (lambda unused
          (or (eq-char char space)
              (lambda unused
                (or (eq-char char tab)
                    (lambda unusedd (eq-char char vtab)))))))))

;; True if a character is part of an identifier.
(def is-identifier-character
  (lambda char
    (not
      (or (is-whitespace char)
          (lambda unused
            (or (eq-char char open-parentheses)
                (lambda unused
                  (eq-char char close-parentheses))))))))

;; Maps an escape code to its character.
(def escape-map
  (lambda char
    (if (eq-char char letter-b) backspace
    (if (eq-char char letter-t) tab
    (if (eq-char char letter-n) linefeed
    (if (eq-char char letter-v) vtab
    (if (eq-char char letter-f) formfeed
    (if (eq-char char letter-r) carriage-return
      char))))))))

;; Reads the contents of a file as a list of characters.
(def file.read (field (symbol file) (symbol read)))

;; The child of a file given a name.
(def file.child (field (symbol file) (symbol child)))

;; The name of a file.
(def file.name (field (symbol file) (symbol name)))

;; The name of a file without its extension.
(def file.name-without-extension
  (lambda file
    (run-with (file.name file)
      (lambda name
        (take-while name
          (lambda char
            (not (eq-char char dot))))))))

;;; Parser

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

;;; Generator

;; The environment of a variable.
(def env
  (lambda vars
    (lambda path
      (lambda def
        (lambda index
          (derive (symbol env) (symbol vars) vars
          (derive (symbol env) (symbol path) path
          (derive (symbol env) (symbol def) def
          (derive (symbol env) (symbol index) index
            (object (symbol env)))))))))))

(def env.vars (field (symbol env) (symbol vars)))
(def env.path (field (symbol env) (symbol path)))
(def env.def (field (symbol env) (symbol def)))
(def env.index (field (symbol env) (symbol index)))

;; The result of generating an expr.
(def generate-result
  (lambda operation
    (lambda declaration
      (lambda env
        (derive (symbol generate-result) (symbol operation) operation
        (derive (symbol generate-result) (symbol declaration) declaration
        (derive (symbol generate-result) (symbol env) env
          (object (symbol generate-result)))))))))

(def generate-result.operation
  (field (symbol generate-result) (symbol operation)))

(def generate-result.declaration
  (field (symbol generate-result) (symbol declaration)))

(def generate-result.env
  (field (symbol generate-result) (symbol env)))

;; The location of a local variable.
(def local-variable
  (lambda name
    (lambda index
      (derive (symbol local-variable) (symbol name) name
      (derive (symbol local-variable) (symbol index) index
        (object (symbol local-variable)))))))

(def local-variable.name (field (symbol local-variable) (symbol name)))
(def local-variable.index (field (symbol local-variable) (symbol index)))

;; Tests if a value is a local variable.
(def is-local-variable
  (lambda x
    (eq-symbol (type-name x) (symbol local-variable))))

;; The location of a global variable
(def global-variable
  (lambda name
    (lambda path
      (derive (symbol global-variable) (symbol name) name
      (derive (symbol global-variable) (symbol path) path
        (object (symbol global-variable)))))))

(def global-variable.name (field (symbol global-variable) (symbol name)))
(def global-variable.path (field (symbol global-variable) (symbol path)))

;; Tests if a value is a global variable.
(def is-global-variable
  (lambda x
    (eq-symbol (type-name x) (symbol global-variable))))

;; A set of closures in an expression.
(def closures
  (lambda expr
    (lambda env1
      (if (is-identifier-expr expr)
        ((lambda variable
          (if (is-none variable)
            (empty-tree-map compare-string)
            (if (is-local-variable (some.value variable))
              (tree-map.put
                (empty-tree-map compare-string)
                (identifier-expr.name expr)
                true)
              (empty-tree-map compare-string))))
        (tree-map.get (env.vars env1) (identifier-expr.name expr)))
        (fold (list-expr.exprs expr) (empty-tree-map compare-string)
          (lambda map
            (lambda expr
              (tree-map.add map (closures expr env1)))))))))

;; Creates a local variable operation.
(def local-variable-operation ())

;; Creates a global variable operation.
(def global-variable-operation ())

;; Creates a reflective variable operation.
(def reflective-variable-operation ())

;; Creates an if operation.
(def if-operation ())

;; Creates a def operation.
(def def-operation ())

;; Creates a def declaration.
(def def-declaration ())

;; Creates a lambda operation.
(def lambda-operation ())

;; Creates a lambda declaration.
(def lambda-declaration ())

;; Creates a symbol operation.
(def symbol-operation ())

;; Creates an apply operation.
(def apply-operation ())

;; The nil operation.
(def nil-operation ())

;; Operation which does nothing.
(def no-operation ())

;; Declaration which does nothing.
(def no-declaration ())

;; Combines two operations.
(def combine-operation ())

;; Ignores the result of an operation.
(def ignore-result-operation ())

;; Marks an operation with a line number.
(def line-number-operation ())

;; Combines two declarations.
(def combine-declaration ())

;; Mangles the name of a lambda given an index.
(def mangle-lambda-name ())

;; Generates a program.
(def generate-program ())

;; List containing all internal variables.
(def internal-variable ())

;; Generates an identifier expression.
(def generate-identifier-expr
  (lambda name
    (lambda env1
      (generate-result
        ((lambda maybe
          (if (is-some maybe)
            ((lambda variable
              (if (is-global-variable variable)
                (global-variable-operation
                  (global-variable.name variable)
                  (global-variable.path variable))
                (local-variable-operation
                  (local-variable.name variable)
                  (local-variable.index variable))))
            (some.value maybe))
            (reflective-variable-operation name (env.path env1))))
          (tree-map.get (env.vars env1) name))
        no-declaration
        env1))))

;; Generates a nil expression.
(def generate-nil
  (lambda env1
    (generate-result nil-operation no-declaration env1)))

;; Generates an if expression.
(def generate-if-expr
  (lambda cond-expr
    (lambda true-expr
      (lambda false-expr
        (lambda env1
          ((lambda cond-result
            ((lambda true-result
              ((lambda false-result
                (generate-result
                  (if-operation
                    (generate-result.operation cond-result)
                    (generate-result.operation true-result)
                    (generate-result.operation false-result))
                  (combine-declaration
                    (generate-result.declaration cond-result)
                    (combine-declaration
                      (generate-result.declaration true-result)
                      (generate-result.declaration false-result)))
                  (generate-result.env false-result)))
              (generate-expr false-expr (generate-result.env true-result))))
            (generate-expr true-expr (generate-result.env cond-result))))
          (generate-expr cond-expr env1)))))))

;; Generates a lambda expression.
(def generate-lambda-expr
  (lambda name
    (lambda expr
      (lambda env1
        ((lambda method-name
          ((lambda env2
            ((lambda closures
              ((lambda expr-result
                (generate-result
                  (lambda-operation
                    (env.path env2)
                    method-name
                    (map closures
                      (lambda closure
                        (generate-result.operation
                          (generate-identifier-expr closure env2)))))
                  (combine-declaration
                    (generate-result.declaration expr-result)
                    (lambda-declaration method-name closures
                      (generate-result.operation expr-result)))
                  env2))
              (generate-expr expr
                (env
                  (second
                    (fold
                      (append closures name)
                      (pair zero (env.vars env2))
                      (lambda vars
                        (lambda closure
                          (pair
                            (add-int one (first vars))
                            (tree-map.put
                              (second vars)
                              closure
                              (local-variable closure (first vars))))))))
                  (env.path env2)
                  (env.def env2)
                  (env.index env1)))))
            (map (tree-map->list (closures expr env2)) first)))
          (env
            (env.vars env1)
            (env.path env1)
            method-name
            (add-int one (env.index env1)))))
        (mangle-lambda-name (env.def env1) (env.index env1)))))))

;; Generates a def expression.
(def generate-def-expr
  (lambda name
    (lambda expr
      (lambda env1
        ((lambda env2
          ((lambda local-env
            ((lambda expr-result
              (if (is-none (tree-map.get (env.vars env1) name))
                (generate-result
                  (def-operation
                    name
                    (generate-result.operation expr-result)
                    (env.path local-env))
                  (combine-declaration
                    (generate-result.declaration expr-result)
                    (def-declaration name (env.path local-env)))
                  env2)
                (generate-result
                  (generate-result.operation
                    (generate-identifier-expr name env1))
                  no-declaration
                  env1)))
            (generate-expr expr local-env)))
          (env (env.vars env2) (env.path env2) name (env.index env2))))
        (env
          (tree-map.put
            (env.vars env1)
            name
            (global-variable name (env.path env1)))
          (env.path env1)
          (env.def env1)
          (env.index env1)))))))

;; Generates a symbol expression.
(def generate-symbol-expr
  (lambda name
    (lambda env1
      (generate-result (symbol-operation name) no-declaration env1))))

;; Generates an apply expression.
(def generate-apply-expr
  (lambda fn
    (lambda args
      (lambda env1
        (if (is-nil args)
          (generate-apply-expr fn (cons (list-expr nil (expr.line fn)) nil) env1)
        (if (is-nil (cdr args))
          ((lambda fn-result
            ((lambda arg-result
              (generate-result
                (apply-operation
                  (generate-result.operation fn-result)
                  (generate-result.operation arg-result))
                (combine-declaration
                  (generate-result.declaration fn-result)
                  (generate-result.declaration arg-result))
                (generate-result.env arg-result)))
            (generate-expr (car args) (generate-result.env fn-result))))
          (generate-expr fn env1))
          (generate-apply-expr
            (list-expr (cons fn (cons (car args) nil)) (expr.line fn))
            (cdr args)
            env1)))))))

;; Generates a list expression.
(def generate-list-expr
  (lambda expr
    (lambda env1
      ((lambda exprs
        (if (is-nil exprs)
          (generate-nil env1)
          ((lambda name
            (if (eq-list eq-char name (symbol->list (symbol if)))
              (generate-if-expr
                (cadr exprs)
                (caddr exprs)
                (cadddr exprs)
                env1)
            (if (eq-list eq-char name (symbol->list (symbol lambda)))
              (generate-lambda-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                env1)
            (if (eq-list eq-char name (symbol->list (symbol def)))
              (generate-def-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                env1)
            (if (eq-list eq-char name (symbol->list (symbol symbol)))
              (generate-symbol-expr
                (identifier-expr.name (cadr exprs))
                env1)
              (generate-apply-expr
                (car exprs)
                (cdr exprs)
                env1))))))
          (if (is-identifier-expr (car exprs))
            (identifier-expr.name (car exprs))
            nil))))
      (list-expr.exprs expr)))))

;; Generates a single expression.
(def generate-expr
  (lambda expr
    (lambda env1
      ((lambda expr-result
        (generate-result
          (line-number-operation
            (generate-result.operation expr-result)
            (expr.line expr))
          (generate-result.declaration expr-result)
          (generate-result.env expr-result)))
      (if (is-identifier-expr expr)
        (generate-identifier-expr (identifier-expr.name expr) env1)
        (generate-list-expr expr env1))))))

;; Generates a list of expressions.
(def generate-exprs
  (lambda exprs
    (lambda env1
      (if (is-nil exprs)
        (generate-result no-operation no-declaration env1)
        ((lambda generate-result-car
          ((lambda generate-result-cdr
            (generate-result
              (combine-operation
                (ignore-result-operation
                  (generate-result.operation generate-result-car))
                (generate-result.operation generate-result-cdr))
              (combine-declaration
                (generate-result.declaration generate-result-car)
                (generate-result.declaration generate-result-cdr))
              (generate-result.env generate-result-cdr)))
          (generate-exprs
            (cdr exprs)
            (generate-result.env generate-result-car))))
        (generate-expr (car exprs) env1))))))

;; Generates the output code given a list of M expressions.
(def generate
  (lambda name
    (lambda out-file
      (lambda exprs
        ((lambda result
          (generate-program
            name
            out-file
            (generate-result.operation result)
            (generate-result.declaration result)))
        (generate-exprs
          exprs
          (env
            (fold internal-variables (empty-tree-map compare-string)
              (lambda map
                (lambda variable
                  (tree-map.put map (first variable) (second variable)))))
            (cons name nil)
            nil
            zero)))))))

;;; Compiler

;; Compiles [in-file], writing the generated code to [out-file].
(def compile
  (lambda in-file
    (lambda out-file
      (then-run-with (file.read in-file)
        (lambda char-stream
          (then-run-with (file.name-without-extension in-file)
            (lambda name
              (generate name out-file
                (parse char-stream)))))))))

(run-unsafe
  (then-run-with (file.child local-file (car args))
    (lambda in-file
      (then-run-with (file.child local-file (cadr args))
        (lambda out-file
          (compile in-file out-file))))))