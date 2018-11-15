(import predef)
(import generate-result)
(import env)
(import expr)
(import variables)
(import operations)
(import declarations)

;; Mangles the name of a lambda given an index.
(def mangle-lambda-name ())

;; Generates a program.
(def generate-program ())

;; List containing all internal variables.
(def internal-variable ())

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
                            (add-nat one (first vars))
                            (tree-map.put
                              (second vars)
                              closure
                              (local-variable closure (first vars))))))))
                  (env.path env2)
                  (env.def env2)
                  (env.index env1)
                  (env.imports env2)))))
            (map (tree-map->list (closures expr env2)) first)))
          (env
            (env.vars env1)
            (env.path env1)
            method-name
            (add-nat one (env.index env1))
            (env.imports env1))))
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
          (env
            (env.vars env2)
            (env.path env2)
            name
            (env.index env2)
            (env.imports env2))))
        (env
          (tree-map.put
            (env.vars env1)
            name
            (global-variable name (env.path env1)))
          (env.path env1)
          (env.def env1)
          (env.index env1)
          (env.imports env1)))))))

;; Generates a symbol expression.
(def generate-symbol-expr
  (lambda name
    (lambda env1
      (generate-result (symbol-operation name) no-declaration env1))))

;; Generates an import expression.
(def generate-import-expr
  (lambda name
    (lambda env1
      (if (is-some (tree-map.get (env.imports env1) name))
        (generate-result (import-operation name) no-declaration env1)
        ((lambda exprs-result
          (generate-result
            (import-operation name)
            (import-declaration name)
            ((lambda env2
              (env
                (env.vars env2)
                (env.path env1)
                (env.def env2)
                (env.index env2)
                (env.imports env2)))
            (generate-result.env exprs-result))))
        (generate-exprs
          (some.value (tree-map.get local-files name))
          (env
            (env.vars env1)
            name
            (env.def env1)
            (env.index env1)
            (tree-map.put (env.imports env1) name ()))))))))

;; Generates an apply expression.
(def generate-apply-expr
  (lambda fn
    (lambda args
      (lambda env1
        (if (is-nil args)
          (generate-apply-expr
            fn
            (cons (list-expr nil (expr.line fn)) nil)
            env1)
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
            (if (eq-list eq-char name (symbol->list (symbol import)))
              (generate-import-expr
                (identifier-expr.name (cadr exprs))
                env1)
              (generate-apply-expr
                (car exprs)
                (cdr exprs)
                env1)))))))
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
                  (tree-map.put map (first variable) (second variable)))))
            name
            nil
            zero
            (tree-map.put (empty-tree-map compare-string) name ()))))))))

(import parser)

;; Reads files from a directory into a map.
(def load-files
  (lambda file
    (lambda path
      (then-run-with (file.directory? file)
        (lambda directory?
          (if directory?
            (then-run-with (file.child-files file)
              (lambda child-files
                (fold child-files
                  (function->process
                    (lambda unused (empty-tree-map compare-string)))
                  (lambda map-process
                    (lambda file
                      (then-run-with map-process
                        (lambda map
                          (then-run-with (file.name-without-extension file)
                            (lambda name
                              (run-with (load-files file
                                          (add-list path (cons dot name)))
                                (lambda load-files
                                  (tree-map.add map load-files))))))))))))
            (run-with (file.read file)
              (lambda chars
                (tree-map.put (empty-tree-map compare-string)
                  (cdr path)
                  (parse chars))))))))))

;; The list of arguments passed to the program.
(def args ())

;; The input file for the compiler.
(def in-file (run-unsafe (file.child local-file (car args))))

;; The output file for the compiler.
(def out-file (run-unsafe (file.child local-file (cadr args))))

;; The tree of local files.
(def local-files (run-unsafe (load-files in-file nil)))