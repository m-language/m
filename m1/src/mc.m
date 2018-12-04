;;; Error

;; Exits the M program given an error.
(def error ())

(def debug ())

;;; Pair

;; Creates a pair of two values.
(def pair ())

;; The left value of a pair.
(def left ())

;; The right value of a pair.
(def right ())

;;; Bool

;; The singleton truthy value, for which `(if true x y)` evaluates to x.
(def true ())

;; The singleton falsy value, for which `(if false x y)` evaluates to y.
(def false ())

;; True if [x] and [y] are true.
(def and
  (lambda x
    (lambda y
      (if x (y ()) false))))

;; True if [x] or [y] is true.
(def or
  (lambda x
    (lambda y
      (if x true (y ())))))

;; True if [x] is false.
(def not
  (lambda x
    (if x false true)))

;;; Symbol

;; Converts a symbol to a list of characters.
(def symbol->list ())

;; Converts a symbol to a nat.
(def symbol->nat ())

;; Converts a symbol to a character.
(def symbol->char ())

;; Tests if two symbols are equal.
(def symbol.= ())

;; Adds two symbols
(def symbol.+ ())

;;; Char

;; Converts a character to a nat.
(def char->nat ())

;; Tests if two characters are equal.
(def char.= ())

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
(def newline?
  (lambda char
    (or (char.= char linefeed)
        (lambda ""
          (or (char.= char carriage-return)
              (lambda ""
                (char.= char formfeed)))))))

;; True if a character is a newline, " ", "\t", or "\v".
(def whitespace?
  (lambda char
    (or (newline? char)
        (lambda ""
          (or (char.= char space)
              (lambda ""
                (or (char.= char tab)
                    (lambda "" (char.= char vtab)))))))))

;;; Nat

;; Adds two nats.
(def nat.+ ())

;; True if the first nat is greater than the second nat.
(def nat.> ())

;; True if the first nat is less than the second nat.
(def nat.< ())

;; True if two nats are equal.
(def nat.= ())

;; Converts a nat to a character.
(def nat->char ())

;; The literal number 0.
(def zero (symbol->nat (symbol 0)))

;; The literal number 1.
(def one (symbol->nat (symbol 1)))

;;; Function

;; Composes two functions [f] and [g].
(def compose
  (lambda f
    (lambda g
      (lambda x
        (f (g x))))))

;; A function which always returns [value].
(def const
  (lambda value
    (lambda "" value)))

;; Applies [f] to [x].
(def with
  (lambda x
    (lambda f
      (f x))))

;;; List

;; The singleton empty list.
(def nil ())

;; Tests if a value is the empty list.
(def nil? ())

;; Prepends an element to a list.
(def cons pair)

;; The first element in a list.
(def car left)

;; The rest of the elements in a list.
(def cdr right)

;; The rest of the rest of the list.
(def cddr (compose cdr cdr))

;; The second element in a list.
(def cadr (compose car cdr))

;; The third element in a list.
(def caddr (compose car cddr))

;; The fourth element in a list.
(def cadddr (compose car (compose cdr cddr)))

;; Appends [elem] to [list].
(def append
  (lambda list
    (lambda elem
      (if (nil? list)
        (cons elem list)
        (cons (car list) (append (cdr list) elem))))))

;; Adds [list1] and [list2].
(def list.+
  (lambda list1
    (lambda list2
      (if (nil? list1)
        list2
        (cons (car list1) (list.+ (cdr list1) list2))))))

;; Maps [list] with function [f].
(def map
  (lambda list
    (lambda f
      (if (nil? list)
        nil
        (cons (f (car list)) (map (cdr list) f))))))

;; Folds [list] with an accumulator [acc] function [f].
(def fold
  (lambda list
    (lambda acc
      (lambda f
        (if (nil? list)
          acc
          (fold (cdr list) (f acc (car list)) f))))))

;; Implementation of reverse.
(def reverse'
  (lambda list
    (lambda acc
      (if (nil? list)
        acc
        (reverse'
          (cdr list)
          (cons (car list) acc))))))

;; Reverses [list].
(def reverse
  (lambda list
    (reverse' list ())))

;; Takes elements of [list] while [f] is true.
(def take-while
  (lambda list
    (lambda f
      (if (nil? list)
        nil
        (if (f (car list))
          (cons (car list) (take-while (cdr list) f))
          nil)))))

;; Tests if [list1] and [list2] are equal given a function [f].
(def list.=
  (lambda f
    (lambda list1
      (lambda list2
        (if (nil? list1)
          (nil? list2)
          (if (nil? list2)
            false
            (and (f (car list1) (car list2))
                 (lambda unused (list.= f (cdr list1) (cdr list2))))))))))

;;; Data

;; The type of a data object.
(def type-name left)

;; Tests if [data] is of [type].
(def is?
  (lambda type
    (lambda data
      (symbol.= type (type-name data)))))

;; Casts [data] to [type].
(def as
  (lambda type
    (lambda data
      (if (is? type data)
        data
        (error (symbol.+ (symbol "Could not cast ")
               (symbol.+ (type-name data)
               (symbol.+ (symbol " to ")
                 type))))))))

;; Derives a data object with a field.
(def derive
  (lambda type
    (lambda field
      (lambda value
        (lambda data
          (pair (left (as type data))
            (lambda name
              (if (symbol.= name field)
                value
                ((right data) name)))))))))

;; Gets a field in a data object.
(def field
  (lambda type
    (lambda name
      (lambda data
        ((right (as type data)) name)))))

;; Creates an empty data object with type [type].
(def object
  (lambda type
    (pair type (const ()))))

;;; Maybe

;; A container for something.
(def some
  (lambda value
    (derive (symbol some) (symbol value) value
      (object (symbol some)))))

;; Tests if a value is a some.
(def some? (is? (symbol some)))

(def some.value (field (symbol some) (symbol value)))

;; The lack of something.
(def none (object (symbol none)))

;; Tests if a value is none.
(def none? (is? (symbol none)))

;;; Process

;; Combines two processes, running them one after another.
(def then-run ())

;; Runs a function in a process.
(def run-with ())

;; Runs a function which produces a process in a process, then combines them.
(def then-run-with ())

;; Runs a process at top level.
(def run-unsafe ())

;; Converts a function to a process.
(def function->process ())

;;; File

;; The root file for this program.
(def file.local-file ())

;; The child of a file given a name.
(def file.child ())

;; A list of child files.
(def file.child-files ())

;; True if a file is a directory.
(def file.directory? ())

;; The name of a file.
(def file.name ())

;; Reads the contents of a file as a list of characters.
(def file.read ())

;; The name of a file without its extension.
(def file.name-without-extension
  (lambda file
    (run-with (file.name file)
      (lambda name
        (take-while name
          (lambda char
            (not (char.= char dot))))))))

;;; Compare

;; Represents that two values are equal.
(def compare= (object (symbol compare=)))

;; Represents that the first value is greater than the second value.
(def compare< (object (symbol compare<)))

;; Represents that the first value is less than the second value.
(def compare> (object (symbol compare>)))

;; Tests if a value is compare=.
(def compare=? (is? (symbol compare=)))

;; Tests if a value is compare<.
(def compare<? (is? (symbol compare<)))

;; Tests if a value is compare>.
(def compare>? (is? (symbol compare>)))

;; Folds over the result of a compare.
(def fold-compare
  (lambda compare
    (lambda <
      (lambda >
        (lambda =
          (if (compare<? compare)
            (< compare)
            (if (compare>? compare)
              (> compare)
              (= compare))))))))

;; Compares lists given [compare].
(def compare-list
  (lambda compare
    (lambda list1
      (lambda list2
        (if (and (nil? list1)
                 (lambda "" (nil? list2)))
          compare=
        (if (nil? list1)
          compare<
        (if (nil? list2)
          compare>
        ((lambda compare-result
          (if (compare=? compare-result)
            (compare-list compare (cdr list1) (cdr list2))
            compare-result))
          (compare (car list1) (car list2))))))))))

;; Compares nats.
(def compare-nat
  (lambda nat1
    (lambda nat2
      (if (nat.> nat1 nat2)
        compare>
        (if (nat.< nat1 nat2)
          compare<
          compare=)))))

;; Compares chars.
(def compare-char
  (lambda char1
    (lambda char2
      (compare-nat (char->nat char1) (char->nat char2)))))

;; Compare strings.
(def compare-string (compare-list compare-char))

;;; Tree map

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
(def tree-map-node-nil? (is? (symbol tree-map-node-nil)))

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
        (if (tree-map-node-nil? node)
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
          (if (tree-map-node-nil? node)
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
        (if (tree-map-node-nil? node)
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

;;; Expr

;; An expression representing an M identifier.
(def identifier-expr
  (lambda name
    (lambda start
      (lambda end
        (derive (symbol identifier-expr) (symbol name) name
        (derive (symbol identifier-expr) (symbol start) start
        (derive (symbol identifier-expr) (symbol end) end
          (object (symbol identifier-expr)))))))))

(def identifier-expr? (is? (symbol identifier-expr)))

(def identifier-expr.name (field (symbol identifier-expr) (symbol name)))
(def identifier-expr.start (field (symbol identifier-expr) (symbol start)))
(def identifier-expr.end (field (symbol identifier-expr) (symbol end)))

;; An expression representing an M list.
(def list-expr
  (lambda exprs
    (lambda start
      (lambda end
        (derive (symbol list-expr) (symbol exprs) exprs
        (derive (symbol list-expr) (symbol start) start
        (derive (symbol list-expr) (symbol end) end
          (object (symbol list-expr)))))))))

(def list-expr? (is? (symbol list-expr)))

(def list-expr.exprs (field (symbol list-expr) (symbol exprs)))
(def list-expr.start (field (symbol list-expr) (symbol start)))
(def list-expr.end (field (symbol list-expr) (symbol end)))

(def expr.start
  (lambda expr
    (if (list-expr? expr)
      (list-expr.start expr)
      (identifier-expr.start expr))))

(def expr.end
  (lambda expr
    (if (list-expr? expr)
      (list-expr.end expr)
      (identifier-expr.end expr))))

;;; Parser

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
  (lambda line
    (lambda char
      (derive (symbol position) (symbol line) line
      (derive (symbol position) (symbol char) char
        (object (symbol position)))))))

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
  (lambda rest
    (lambda expr
      (derive (symbol parse-result) (symbol rest) rest
      (derive (symbol parse-result) (symbol expr) expr
        (object (symbol parse-result)))))))

(def parse-result.rest (field (symbol parse-result) (symbol rest)))
(def parse-result.expr (field (symbol parse-result) (symbol expr)))

;; Parses an M commment.
(def parse-comment
  (lambda input
    (lambda position
      (if (newline? (car input))
        (parse-expr input position)
        (parse-comment (cdr input) position)))))

;; Parses an M identifier literal expression given [input].
(def parse-identifier-literal-expr
  (lambda input
    (lambda start
      (lambda end
        (lambda acc
          (with (car input)
            (lambda head
              (if (char.= head quote)
                (parse-result
                  (cdr input)
                  (identifier-expr (reverse acc) start end))
              (if (char.= head backslash)
                (parse-identifier-literal-expr
                  (cddr input)
                  start
                  (next-char (next-char end))
                  (cons (escape (cadr input)) acc))
              (if (newline? head)
                (parse-identifier-literal-expr
                  (cdr input)
                  start
                  (next-line end)
                  (cons head acc))
                (parse-identifier-literal-expr
                  (cdr input)
                  start
                  (next-char end)
                  (cons head acc))))))))))))

;; Parses an M identifier expression given [input].
(def parse-identifier-expr
  (lambda input
    (lambda start
      (lambda end
        (lambda acc
          (if (separator? (car input))
            (parse-result input (identifier-expr (reverse acc) start end))
            (parse-identifier-expr
              (cdr input)
              start
              (next-char end)
              (cons (car input) acc))))))))

;; Parses an M list expression given [input].
(def parse-list-expr
  (lambda input
    (lambda start
      (lambda end
        (lambda acc
          (if (char.= (car input) close-parentheses)
            (parse-result (cdr input) (list-expr (reverse acc) start end))
            (with (parse-expr input end)
              (lambda result
                (parse-list-expr
                  (parse-result.rest result)
                  start
                  (expr.end (parse-result.expr result))
                  (cons (parse-result.expr result) acc))))))))))

;; Parses an M expression given [input].
(def parse-expr
  (lambda input
    (lambda position
      (with (car input)
        (lambda head
          (if (char.= head open-parentheses)
            (parse-list-expr (cdr input) position position ())
          (if (char.= head quote)
            (parse-identifier-literal-expr (cdr input) position position ())
          (if (char.= head semicolon)
            (parse-comment (cdr input) position)
          (if (newline? head)
            (parse-expr (cdr input) (next-line position))
          (if (whitespace? head)
            (parse-expr (cdr input) (next-char position))
            (parse-identifier-expr input position position ())))))))))))

;; Parses an M program given [input].
(def parse
  (lambda input
    (lambda position
      (lambda acc
        (if (nil? input)
          (reverse acc)
          (with (parse-expr input position)
            (lambda result
              (parse
                (parse-result.rest result)
                (expr.end (parse-result.expr result))
                (cons (parse-result.expr result) acc)))))))))

;;; Generate Result

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

;;; Variables

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
(def local-variable? (is? (symbol local-variable)))

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
(def global-variable? (is? (symbol global-variable)))

;;; Env

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

;;; Operations

;; Operation which pushes a local variable.
(def local-variable-operation
  (lambda name
    (lambda index
      (derive (symbol local-variable-operation) (symbol name) name
      (derive (symbol local-variable-operation) (symbol index) index
        (object (symbol local-variable-operation)))))))

;; Operation which pushes a global variable.
(def global-variable-operation
  (lambda name
    (lambda path
      (derive (symbol global-variable-operation) (symbol name) name
      (derive (symbol global-variable-operation) (symbol path) path
        (object (symbol global-variable-operation)))))))

;; Operation which pushes a reflective variable.
(def reflective-variable-operation
  (lambda name
    (lambda path
      (derive (symbol reflective-variable-operation) (symbol name) name
      (derive (symbol reflective-variable-operation) (symbol path) path
        (object (symbol reflective-variable-operation)))))))

;; An if operation.
(def if-operation
  (lambda cond
    (lambda true
      (lambda false
        (derive (symbol if-operation) (symbol cond) cond
        (derive (symbol if-operation) (symbol true) true
        (derive (symbol if-operation) (symbol false) false
          (object (symbol if-operation)))))))))

;; A def operation.
(def def-operation
  (lambda name
    (lambda value
      (lambda path
        (derive (symbol def-operation) (symbol name) name
        (derive (symbol def-operation) (symbol value) value
        (derive (symbol def-operation) (symbol path) path
          (object (symbol def-operation)))))))))

;; A lambda operation.
(def lambda-operation
  (lambda path
    (lambda name
      (lambda closures
        (derive (symbol lambda-operation) (symbol path) path
        (derive (symbol lambda-operation) (symbol name) name
        (derive (symbol lambda-operation) (symbol closures) closures
          (object (symbol lambda-operation)))))))))

;; A symbol operation.
(def symbol-operation
  (lambda name
    (derive (symbol symbol-operation) (symbol name) name
      (object (symbol symbol-operation)))))

;; An apply operation.
(def apply-operation
  (lambda fn
    (lambda arg
      (derive (symbol apply-operation) (symbol fn) fn
      (derive (symbol apply-operation) (symbol arg) arg
        (object (symbol apply-operation)))))))

;; Combines two operations.
(def combine-operation
  (lambda first
    (lambda second
      (derive (symbol combine-operation) (symbol first) first
      (derive (symbol combine-operatoin) (symbol second) second
        (object (symbol combine-operation)))))))

;; Marks an operation with a line number.
(def line-number-operation
  (lambda operation
    (lambda line
      (derive (symbol line-number-operation) (symbol operation) operation
      (derive (symbol line-number-operation) (symbol line) line
        (object (symbol line-number-operation)))))))

;; The nil operation.
(def nil-operation (object (symbol nil-operation)))

;;; Declarations

;; A def declaration.
(def def-declaration
  (lambda name
    (lambda path
      (lambda value
        (derive (symbol def-declaration) (symbol name) name
        (derive (symbol def-declaration) (symbol path) path
        (derive (symbol def-declaration) (symbol value) value
          (object (symbol def-declaration)))))))))

;; A lambda declaration.
(def lambda-declaration
  (lambda name
    (lambda closures
      (lambda value
        (derive (symbol lambda-declaration) (symbol name) name
        (derive (symbol lambda-declaration) (symbol closures) closures
        (derive (symbol lambda-declaration) (symbol value) value
          (object (symbol lambda-declaration)))))))))

;; Combines two declarations.
(def combine-declaration
  (lambda first
    (lambda second
      (derive (symbol combine-declaration) (symbol first) first
      (derive (symbol combine-declaration) (symbol second) second
        (object (symbol combine-declaration)))))))

;; Declaration which does nothing.
(def no-declaration (object (symbol no-declaration)))

;;; Generator

;; Mangles the name of a lambda given an index.
(def mangle-lambda-name ())

;; Generates a program.
(def generate-program ())

;; List containing all internal variables.
(def internal-variables ())

;; A set of closures in an expression.
(def closures
  (lambda expr
    (lambda env1
      (if (identifier-expr? expr)
        ((lambda variable
          (if (none? variable)
            (empty-tree-map compare-string)
            (if (local-variable? (some.value variable))
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

;; Generates an identifier expression.
(def generate-identifier-expr
  (lambda name
    (lambda env1
      (generate-result
        ((lambda maybe
          (if (some? maybe)
            ((lambda variable
              (if (global-variable? variable)
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
                  (right
                    (fold
                      (append closures name)
                      (pair zero (env.vars env2))
                      (lambda vars
                        (lambda closure
                          (pair
                            (nat.+ one (left vars))
                            (tree-map.put
                              (right vars)
                              closure
                              (local-variable closure (left vars))))))))
                  (env.path env2)
                  (env.def env2)
                  (env.index env1)))))
            (map (tree-map->list (closures expr env2)) left)))
          (env
            (env.vars env1)
            (env.path env1)
            method-name
            (nat.+ one (env.index env1)))))
        (mangle-lambda-name (env.def env1) (env.index env1)))))))

;; Generates a def expression.
(def generate-def-expr
  (lambda name
    (lambda expr
      (lambda env1
        ((lambda env2
          ((lambda local-env
            ((lambda expr-result
              (if (none? (tree-map.get (env.vars env1) name))
                (generate-result
                  (def-operation
                    name
                    (generate-result.operation expr-result)
                    (env.path local-env))
                  (combine-declaration
                    (generate-result.declaration expr-result)
                    (def-declaration name (env.path local-env)
                      (generate-result.operation expr-result)))
                  env2)
                (generate-result
                  (generate-result.operation
                    (generate-identifier-expr name env1))
                  no-declaration
                  env1)))
            (generate-expr expr local-env)))
          (env
            (env.vars env2)
            (env.path env2)
            name
            (env.index env2))))
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
        (if (nil? args)
          (generate-apply-expr
            fn
            (cons (list-expr nil (expr.line fn)) nil)
            env1)
        (if (nil? (cdr args))
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
            (list-expr
              (cons fn (cons (car args) nil))
              (expr.start fn)
              (expr.end fn))
            (cdr args)
            env1)))))))

;; Generates a list expression.
(def generate-list-expr
  (lambda expr
    (lambda env1
      ((lambda exprs
        (if (nil? exprs)
          (generate-nil env1)
          ((lambda name
            (if (list.= char.= name (symbol->list (symbol if)))
              (generate-if-expr
                (cadr exprs)
                (caddr exprs)
                (cadddr exprs)
                env1)
            (if (list.= char.= name (symbol->list (symbol lambda)))
              (generate-lambda-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                env1)
            (if (list.= char.= name (symbol->list (symbol def)))
              (generate-def-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                env1)
            (if (list.= char.= name (symbol->list (symbol symbol)))
              (generate-symbol-expr
                (identifier-expr.name (cadr exprs))
                env1)
              (generate-apply-expr
                (car exprs)
                (cdr exprs)
                env1))))))
          (if (identifier-expr? (car exprs))
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
            (position.line (expr.start expr)))
          (generate-result.declaration expr-result)
          (generate-result.env expr-result)))
      (if (identifier-expr? expr)
        (generate-identifier-expr (identifier-expr.name expr) env1)
        (generate-list-expr expr env1))))))

;; Generates a list of expressions.
(def generate-exprs
  (lambda exprs
    (lambda env1
      (if (nil? exprs)
        (generate-result nil-operation no-declaration env1)
        ((lambda generate-result-car
          ((lambda generate-result-cdr
            (generate-result
              (combine-operation
                (generate-result.operation generate-result-car)
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
                  (tree-map.put map (left variable) (right variable)))))
            name
            nil
            zero)))))))

;;; MC

;; The list of arguments passed to the program.
(def args ())

(run-unsafe
  (then-run-with (file.child file.local-file (car args))
    (lambda in-file
      (then-run-with (file.child file.local-file (cadr args))
        (lambda out-file
          (then-run-with (file.read in-file)
            (lambda chars
              (then-run-with (file.name-without-extension in-file)
                (lambda name
                  (generate name out-file
                    (parse chars (position one one) ())))))))))))