(def undefined ())
(def true ())
(def false ())
(def nil ())
(def cons ())
(def car ())
(def cdr ())
(def symbol->list ())
(def add-int ())
(def gt-int ())
(def lt-int ())
(def symbol->int ())
(def char->int ())
(def eq-char ())
(def int->char ())
(def symbol->char ())
(def eq-symbol ())
(def type-name ())
(def object ())
(def derive ())
(def field ())
(def then-run ())
(def run-with ())
(def then-run-with ())
(def run-unsafe ())
(def file ())
(def args ())

(def pair
  (lambda first
    (lambda second
      (derive (symbol pair) (symbol first) first
      (derive (symbol pair) (symbol second) second
        (object (symbol pair)))))))

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
(def cddr (compose cdr cdr))
(def caddr (compose car cddr))
(def cadddr (compose car (compose cdr cddr)))

(def append
  (lambda list
    (lambda elem
      (if (is-nil list)
        (cons elem list)
        (cons (car list) (append (cdr list) elem))))))

(def map
  (lambda list
    (lambda f
      (if (is-nil list)
        nil
        (cons (f (car list)) (map (cdr list) f))))))

(def fold
  (lambda list
    (lambda acc
      (lambda f
        (if (is-nil list)
          acc
          (fold (cdr list) (f acc (car list)) f))))))

(def take-while
  (lambda list
    (lambda f
      (if (is-nil list)
        nil
        (if (f (car list))
          (cons (car list) (take-while (cdr list) f))
          nil)))))

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

(def some
  (lambda value
    (derive (symbol some) (symbol value) value
      (object (symbol some)))))

(def is-some
  (lambda x
    (eq-symbol (type-name x) (symbol some))))

(def some.value (field (symbol some) (symbol value)))

(def none (object (symbol none)))

(def is-none
  (lambda x
    (eq-symbol (type-name x) (symbol none))))

(def compare= (object (symbol compare=)))
(def compare< (object (symbol compare<)))
(def compare> (object (symbol compare>)))

(def is-compare=
  (lambda x
    (eq-symbol (type-name x) (symbol compare=))))

(def is-compare<
  (lambda x
    (eq-symbol (type-name x) (symbol compare<))))

(def is-compare>
  (lambda x
    (eq-symbol (type-name x) (symbol compare>))))

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

(def compare-int
  (lambda int1
    (lambda int2
      (if (gt-int int1 int2)
        compare>
        (if (lt-int int1 int2)
          compare<
          compare=)))))

(def compare-char
  (lambda char1
    (lambda char2
      (compare-int (char->int char1) (char->int char2)))))

(def compare-string (compare-list compare-char))

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

(def tree-map-node-nil (object (symbol tree-map-node-nil)))

(def is-tree-map-node-nil
  (lambda x
    (eq-symbol (type-name x) (symbol tree-map-node-nil))))

(def tree-map
  (lambda node
    (lambda compare
      (derive (symbol tree-map) (symbol node) node
      (derive (symbol tree-map) (symbol compare) compare
        (object (symbol tree-map)))))))

(def tree-map.node (field (symbol tree-map) (symbol node)))
(def tree-map.compare (field (symbol tree-map) (symbol compare)))

(def tree-map
  (lambda node
    (lambda compare
      (tree-map (cons node (cons compare nil))))))

(def empty-tree-map
  (lambda compare
    (tree-map tree-map-node-nil compare)))

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

(def tree-map.get
  (lambda map
    (lambda key
      (tree-map-node.get (tree-map.node map) (tree-map.compare map) key))))

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

(def tree-map.fold
  (lambda map
    (lambda acc
      (lambda f
        (tree-map-node.fold (tree-map.node map) acc f)))))

(def tree-map.add
  (lambda map1
    (lambda map2
      (tree-map.fold map1 map2
        (lambda map
          (lambda key
            (lambda value
              (tree-map.put map key value))))))))

(def tree-map->list
  (lambda map
    (tree-map.fold map nil
      (lambda list
        (lambda key
          (lambda value
            (cons (pair key value) list)))))))

(def parse-failure
  (lambda state
    (derive (symbol parse-failure) (symbol state) state
      (object (symbol parse-failure)))))

(def parse-failure.state (field (symbol parse-failure) (symbol state)))

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

(def is-parse-success
  (lambda x
    (eq-symbol (type-name x) (symbol parse-success))))

(def predicate-parser
  (lambda f
    (lambda input
      (lambda state
        (if (and (not (is-nil input))
                 (lambda unused (f (car input))))
          (parse-success (car input) state (cdr input))
          (parse-failure state))))))

(def success-parser (predicate-parser (lambda unused true)))

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
          (parse-success
            (f (parse-success.value success))
            (parse-success.state success)
            (parse-success.rest success)))))))

(def map-parser-state
  (lambda parser
    (lambda f
      (map-parser-success parser
        (lambda success
          (parse-success
            (parse-success.value success)
            (f (parse-success.state success))
            (parse-success.rest success)))))))

(def provide-past-state
  (lambda parser
    (lambda input
      (lambda state
        ((map-parser-value parser
          (lambda value
            (pair value state)))
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
  (lambda name
    (lambda line
      (derive (symbol identifier-expr) (symbol name) name
      (derive (symbol identifier-expr) (symbol line) line
        (object (symbol identifier-expr)))))))

(def is-identifier-expr
  (lambda x
    (eq-symbol (type-name x) (symbol identifier-expr))))

(def identifier-expr.name (field (symbol identifier-expr) (symbol name)))
(def identifier-expr.line (field (symbol identifier-expr) (symbol line)))

(def list-expr
  (lambda exprs
    (lambda line
      (derive (symbol list-expr) (symbol exprs) exprs
      (derive (symbol list-expr) (symbol line) line
        (object (symbol list-expr)))))))

(def is-list-expr
  (lambda x
    (eq-symbol (type-name x) (symbol list-expr))))

(def list-expr.exprs (field (symbol list-expr) (symbol exprs)))
(def list-expr.line (field (symbol list-expr) (symbol line)))

(def expr.line
  (lambda expr
    (if (is-identifier-expr expr)
      (identifier-expr.line expr)
      (list-expr.line expr))))

(def zero (symbol->int (symbol 0)))
(def one (symbol->int (symbol 1)))

(def symbol->int->char (compose int->char symbol->int))

(def open-parentheses (symbol->int->char (symbol 40)))
(def close-parentheses (symbol->int->char (symbol 41)))
(def semicolon (symbol->int->char (symbol 59)))
(def dot (symbol->int->char (symbol 46)))
(def quote (symbol->int->char (symbol 34)))
(def backslash (symbol->int->char (symbol 92)))
(def space (symbol->int->char (symbol 32)))
(def backspace (symbol->int->char (symbol 8)))
(def tab (symbol->int->char (symbol 9)))
(def linefeed (symbol->int->char (symbol 10)))
(def vtab (symbol->int->char (symbol 11)))
(def formfeed (symbol->int->char (symbol 12)))
(def carriage-return (symbol->int->char (symbol 13)))
(def letter-b (symbol->int->char (symbol 98)))
(def letter-t (symbol->int->char (symbol 116)))
(def letter-n (symbol->int->char (symbol 110)))
(def letter-v (symbol->int->char (symbol 118)))
(def letter-f (symbol->int->char (symbol 102)))
(def letter-r (symbol->int->char (symbol 114)))

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
                    (lambda unusedd (eq-char char vtab)))))))))

(def is-identifier-character
  (lambda char
    (not
      (or (is-whitespace char)
          (lambda unused
            (or (eq-char char open-parentheses)
                (lambda unused
                  (eq-char char close-parentheses))))))))

(def escape-map
  (lambda char
    (if (eq-char char letter-b) backspace
    (if (eq-char char letter-t) tab
    (if (eq-char char letter-n) linefeed
    (if (eq-char char letter-v) vtab
    (if (eq-char char letter-f) formfeed
    (if (eq-char char letter-r) carriage-return
      char))))))))

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
    (predicate-parser (eq-char semicolon))
    (repeat-parser (predicate-parser (compose not is-newline)))))

(def ignore-unused
  (lambda parser
    (combine-parser-right
      (repeat-parser (alternative-parser whitespace-parser comment-parser))
      parser)))

(def identifier-char-parser
  (predicate-parser is-identifier-character))

(def identifier-literal-escape-parser
  (combine-parser-right (char-parser backslash)
    (map-parser-value success-parser escape-map)))

(def identifier-literal-char-parser
  (predicate-parser (compose not (eq-char quote))))

(def identifier-literal-parser
  (combine-parser-right
    (char-parser quote)
    (combine-parser-left
      (repeat-parser
        (alternative-parser
          identifier-literal-escape-parser
          identifier-literal-char-parser))
      (char-parser quote))))

(def identifier-expr-parser
  (ignore-unused
    (map-parser-value
      (provide-past-state
        (alternative-parser
          identifier-literal-parser
          (repeat-parser1 identifier-char-parser)))
      (lambda pair
        (identifier-expr (first pair) (second pair))))))

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

(def env
  (lambda vars
    (lambda file
      (lambda def
        (lambda index
          (derive (symbol env) (symbol vars) vars
          (derive (symbol env) (symbol file) file
          (derive (symbol env) (symbol def) def
          (derive (symbol env) (symbol index) index
            (object (symbol env)))))))))))

(def env.vars (field (symbol env) (symbol vars)))
(def env.file (field (symbol env) (symbol file)))
(def env.def (field (symbol env) (symbol def)))
(def env.index (field (symbol env) (symbol index)))

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

(def local-variable
  (lambda name
    (lambda index
      (derive (symbol local-variable) (symbol name) name
      (derive (symbol local-variable) (symbol index) index
        (object (symbol local-variable)))))))

(def local-variable.name (field (symbol local-variable) (symbol name)))
(def local-variable.index (field (symbol local-variable) (symbol index)))

(def is-local-variable
  (lambda x
    (eq-symbol (type-name x) (symbol local-variable))))

(def global-variable
  (lambda name
    (lambda file
      (derive (symbol global-variable) (symbol name) name
      (derive (symbol global-variable) (symbol file) file
        (object (symbol global-variable)))))))

(def global-variable.name (field (symbol global-variable) (symbol name)))
(def global-variable.file (field (symbol global-variable) (symbol file)))

(def is-global-variable
  (lambda x
    (eq-symbol (type-name x) (symbol global-variable))))

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

(def local-variable-operation ())
(def global-variable-operation ())
(def reflective-variable-operation ())
(def if-operation ())
(def def-operation ())
(def def-declaration ())
(def lambda-operation ())
(def lambda-declaration ())
(def symbol-operation ())
(def apply-operation ())
(def nil-operation ())
(def no-operation ())
(def no-declaration ())
(def combine-operation ())
(def ignore-result-operation ())
(def line-number-operation ())
(def combine-declaration ())
(def mangle-lambda-name ())
(def generate-file ())
(def internal-variable ())

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
                  (global-variable.file variable))
                (local-variable-operation
                  (local-variable.name variable)
                  (local-variable.index variable))))
            (some.value maybe))
            (reflective-variable-operation name (env.file env1))))
          (tree-map.get (env.vars env1) name))
        no-declaration
        env1))))

(def generate-nil
  (lambda env1
    (generate-result nil-operation no-declaration env1)))

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
                    (env.file env2)
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
                  (env.file env2)
                  (env.def env2)
                  (env.index env1)))))
            (map (tree-map->list (closures expr env2)) first)))
          (env
            (env.vars env1)
            (env.file env1)
            method-name
            (add-int one (env.index env1)))))
        (mangle-lambda-name (env.def env1) (env.index env1)))))))

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
                    (env.file local-env))
                  (combine-declaration
                    (generate-result.declaration expr-result)
                    (def-declaration name (env.file local-env)))
                  env2)
                (generate-result
                  (generate-result.operation
                    (generate-identifier-expr name env1))
                  no-declaration
                  env1)))
            (generate-expr expr local-env)))
          (env (env.vars env2) (env.file env2) name (env.index env2))))
        (env
          (tree-map.put
            (env.vars env1)
            name
            (global-variable name (env.file env1)))
          (env.file env1)
          (env.def env1)
          (env.index env1)))))))

(def generate-symbol-expr
  (lambda name
    (lambda env1
      (generate-result (symbol-operation name) no-declaration env1))))

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

(def generate
  (lambda name
    (lambda out-file
      (lambda exprs
        ((lambda result
          (generate-file
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
  (compile
    (file (car args))
    (file (cadr args))))