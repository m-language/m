;; Mangles the name of a function given an index.
(def mangle-fn-name ())

;; The default M environment.
(def default-env
  (fn exprs
    (env
      exprs
      (empty-tree-map compare-symbol)
      (empty-tree-map compare-symbol)
      empty-heap
      ()
      nat.0)))

;; A set of closures in an expression.
(def closures
  (fn expr
    (fn env'
      (if (identifier-expr? expr)
        ((fn variable
          (if (null? variable)
            (empty-tree-map compare-symbol)
            (if (local-variable? (unnull variable))
              (tree-map.put
                (empty-tree-map compare-symbol)
                (identifier-expr.name expr)
                true)
              (empty-tree-map compare-symbol))))
        (env.get env' (identifier-expr.name expr)))
        (fold (list-expr.exprs expr) (empty-tree-map compare-symbol)
          (fn map
            (fn expr
              (tree-map.+ map (closures expr env')))))))))

;; The environment of a variable.
(def env
  (new-data (symbol env)
    (list6
      (symbol exprs)
      (symbol locals)
      (symbol globals)
      (symbol heap)
      (symbol def)
      (symbol index))))

(def env.exprs (field (symbol env) (symbol exprs)))
(def env.locals (field (symbol env) (symbol locals)))
(def env.globals (field (symbol env) (symbol globals)))
(def env.heap (field (symbol env) (symbol heap)))
(def env.def (field (symbol env) (symbol def)))
(def env.index (field (symbol env) (symbol index)))

;; Gets the variable with a name in an environment.
(def env.get
  (fn env'
    (fn name
      (with (tree-map.get (env.locals env') name)
        (fn option
          (if (some? option)
            option
            (tree-map.get (env.globals env') name)))))))

;; The result of generating an expr.
(def generate-result
  (new-data (symbol generate-result)
    (list3 (symbol operation) (symbol declarations) (symbol env))))

(def generate-result.operation
  (field (symbol generate-result) (symbol operation)))

(def generate-result.declarations
  (field (symbol generate-result) (symbol declarations)))

(def generate-result.env
  (field (symbol generate-result) (symbol env)))

;; Generates an identifier expression.
(def generate-identifier-expr
  (fn name
    (fn env'
      (with (env.get env' name)
        (fn option
        (if (some? option)
          (with (unnull option)
          (fn variable
            (if (global-variable? variable)
              (generate-result
                (global-variable-operation
                  (global-variable.name variable)
                  (global-variable.path variable))
                ()
                env')
              (generate-result
                (local-variable-operation
                  (local-variable.name variable)
                  (local-variable.index variable))
                ()
                env'))))
          (if (nil? (env.exprs env'))
            (error (concat (symbol->list (symbol "Could not find variable \""))
                      (concat name
                        (symbol->list (symbol "\"")))))
            (with
              (generate-expr
                (car (env.exprs env'))
                (env
                  (cdr (env.exprs env'))
                  (empty-tree-map compare-symbol)
                  (env.globals env')
                  (env.heap env')
                  ()
                  nat.0))
            (fn next
              (with
                (generate-identifier-expr
                  name
                  (env
                    (env.exprs (generate-result.env next))
                    (env.locals env')
                    (env.globals (generate-result.env next))
                    (env.heap env')
                    (env.def env')
                    (env.index env')))
              (fn result
                (generate-result
                  (combine-operation
                    (generate-result.operation next)
                    (generate-result.operation result))
                  (concat
                    (generate-result.declarations next)
                    (generate-result.declarations result))
                  (generate-result.env result)))))))))))))

;; Generates a nil expression.
(def generate-nil
  (fn env'
    (generate-result nil-operation () env')))

;; Generates an if expression.
(def generate-if-expr
  (fn cond-expr
    (fn true-expr
      (fn false-expr
        (fn env'
          ((fn cond-result
            ((fn true-result
              ((fn false-result
                (generate-result
                  (if-operation
                    (generate-result.operation cond-result)
                    (generate-result.operation true-result)
                    (generate-result.operation false-result))
                  (concat
                    (generate-result.declarations cond-result)
                    (concat
                      (generate-result.declarations true-result)
                      (generate-result.declarations false-result)))
                  (generate-result.env false-result)))
              (generate-expr false-expr (generate-result.env true-result))))
            (generate-expr true-expr (generate-result.env cond-result))))
          (generate-expr cond-expr env')))))))

;; Generates a fn expression.
(def generate-fn-expr
  (fn name
    (fn expr
      (fn env'
        (with (mangle-fn-name (env.def env') (env.index env'))
        (fn mangled-name
          (with
            (env
              (env.exprs env')
              (env.locals env')
              (env.globals env')
              (env.heap env')
              (env.def env')
              (nat.+ nat.1 (env.index env')))
          (fn new-env
            (with (map (tree-map->list (closures expr new-env)) first)
            (fn closures
              (with
                (generate-expr expr
                  (env
                    (env.exprs new-env)
                    (second
                      (fold
                        (append closures name)
                        (pair nat.0 (env.locals new-env))
                        (fn vars
                          (fn closure
                            (pair
                              (nat.+ nat.1 (first vars))
                              (tree-map.put
                                (second vars)
                                closure
                                (local-variable closure (first vars))))))))
                    (env.globals new-env)
                    (env.heap new-env)
                    mangled-name
                    (env.index new-env)))
              (fn result
                (with
                  (fn-declaration
                    mangled-name
                    (expr.path expr)
                    closures
                    (generate-result.operation result))
                (fn declaration
                  (generate-result
                    (fn-operation
                      (expr.path expr)
                        mangled-name
                        (map closures
                        (fn closure
                          (generate-result.operation
                            (generate-identifier-expr closure new-env)))))
                    (append (generate-result.declarations result) declaration)
                    (env
                      (env.exprs (generate-result.env result))
                      (env.locals new-env)
                      (env.globals (generate-result.env result))
                      (interpret-fn-declaration declaration
                        (env.heap (generate-result.env result)))
                      (env.def new-env)
                      (env.index new-env)))))))))))))))))

;; Generates a def expression.
(def generate-def-expr
  (fn name
    (fn expr
      (fn env'
        (if (some? (env.get env' name))
            (error (concat name
                      (symbol->list (symbol " has already been defined"))))
          (with
            (env
              (env.exprs env')
              (env.locals env')
              (tree-map.put
                (env.globals env')
                name
                (global-variable name (expr.path expr)))
              (env.heap env')
              (env.def env')
              (env.index env'))
          (fn new-env
            (with
              (generate-expr
                expr
                (env
                  (env.exprs new-env)
                  (env.locals new-env)
                  (env.globals new-env)
                  (env.heap new-env)
                  name
                  (env.index new-env)))
            (fn result
              (with
                (def-declaration
                  name
                  (expr.path expr)
                  (generate-result.operation result))
              (fn declaration
                (generate-result
                  (def-operation
                    name
                    (expr.path expr)
                    (generate-result.operation result))
                  (cons declaration (generate-result.declarations result))
                  (env
                    (env.exprs (generate-result.env result))
                    (env.locals (generate-result.env result))
                    (env.globals (generate-result.env result))
                    (interpret-def-declaration declaration
                      (env.heap (generate-result.env result)))
                    (env.def new-env)
                    (env.index (generate-result.env result)))))))))))))))

;; Generates a impure expression.
(def generate-impure-expr
  (fn expr
    (fn env'
      (with (generate-expr expr env')
      (fn result
        (generate-result
          (impure-operation (generate-result.operation result))
          (generate-result.declarations result)
          (generate-result.env result)))))))

;; Generates a macro expression.
(def generate-macro-expr
  (fn macro
    (error (symbol "TODO macros"))))

;; Generates a symbol expression.
(def generate-symbol-expr
  (fn name
    (fn env'
      (generate-result (symbol-operation name) () env'))))

;; Generates an apply expression.
(def generate-apply-expr
  (fn fn
    (fn args
      (fn env'
        (if (nil? args)
          (generate-apply-expr
            fn
            (cons
              (list-expr () (expr.path fn) (expr.start fn) (expr.end fn))
              ())
            env')
        (if (nil? (cdr args))
          ((fn fn-result
            ((fn arg-result
              (generate-result
                (apply-operation
                  (generate-result.operation fn-result)
                  (generate-result.operation arg-result))
                (concat
                  (generate-result.declarations fn-result)
                  (generate-result.declarations arg-result))
                (generate-result.env arg-result)))
            (generate-expr (car args) (generate-result.env fn-result))))
          (generate-expr fn env'))
          (generate-apply-expr
            (list-expr
              (list2 fn (car args))
              (expr.path fn)
              (expr.start fn)
              (expr.end fn))
            (cdr args)
            env')))))))

;; Generates a list expression.
(def generate-list-expr
  (fn expr
    (fn env'
      ((fn exprs
        (if (nil? exprs)
          (generate-nil env')
          ((fn name
            (if (list.= char.= name (symbol->list (symbol if)))
              (generate-if-expr
                (cadr exprs)
                (caddr exprs)
                (cadddr exprs)
                env')
            (if (list.= char.= name (symbol->list (symbol fn)))
              (generate-fn-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                env')
            (if (list.= char.= name (symbol->list (symbol def)))
              (generate-def-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                env')
            (if (list.= char.= name (symbol->list (symbol impure)))
              (generate-impure-expr
                (cadr exprs)
                env')
            (if (list.= char.= name (symbol->list (symbol macro)))
              (generate-macro-expr
                (cadr exprs)
                env')
            (if (list.= char.= name (symbol->list (symbol symbol)))
              (generate-symbol-expr
                (identifier-expr.name (cadr exprs))
                env')
              (generate-apply-expr
                (car exprs)
                (cdr exprs)
                env'))))))))
          (if (identifier-expr? (car exprs))
            (identifier-expr.name (car exprs))
            ()))))
      (list-expr.exprs expr)))))

;; Generates a single expression.
(def generate-expr
  (fn expr
    (fn env'
      ((fn expr-result
        (generate-result
          (line-number-operation
            (generate-result.operation expr-result)
            (position.line (expr.start expr)))
          (generate-result.declarations expr-result)
          (generate-result.env expr-result)))
      (if (identifier-expr? expr)
        (generate-identifier-expr (identifier-expr.name expr) env')
        (generate-list-expr expr env'))))))

;; Generates an M environment.
(def generate-env
  (fn env'
    (if (nil? (env.exprs env'))
      (generate-result nil-operation () env')
      (with
        (generate-expr
          (car (env.exprs env'))
          (env
            (cdr (env.exprs env'))
            (env.locals env')
            (env.globals env')
            (env.heap env')
            (env.def env')
            (env.index env')))
      (fn car-result
        (with (generate-env (generate-result.env car-result))
        (fn cdr-result
          (generate-result
            (combine-operation
              (generate-result.operation car-result)
              (generate-result.operation cdr-result))
            (concat
              (generate-result.declarations car-result)
              (generate-result.declarations cdr-result))
            (generate-result.env cdr-result)))))))))