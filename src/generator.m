;; Mangles the name of a function given an index.
(def mangle-fn-name ())

;; The default M environment.
(def default-env
  (fn exprs
    (ap env
      exprs
      (ap empty-tree-map compare-symbol)
      (ap empty-tree-map compare-symbol)
      (ap empty-tree-map compare-symbol)
      ()
      nat.0)))

;; A set of closures in an expression.
(def closures
  (fn expr
    (fn env'
      (if (ap identifier-expr? expr)
        (ap (fn variable
          (if (ap null? variable)
            (ap empty-tree-map compare-symbol)
            (if (ap local-variable? (ap unnull variable))
              (ap tree-map.put
                (ap empty-tree-map compare-symbol)
                (ap identifier-expr.name expr)
                true)
              (ap empty-tree-map compare-symbol))))
        (ap env.get env' (ap identifier-expr.name expr)))
        (ap fold (ap list-expr.exprs expr) (ap empty-tree-map compare-symbol)
          (fn map
            (fn expr
              (ap tree-map.+ map (ap closures expr env')))))))))

;; The environment of a variable.
(def env
  (ap new-data (symbol env)
    (ap list6
      (symbol exprs)
      (symbol locals)
      (symbol globals)
      (symbol heap)
      (symbol def)
      (symbol index))))

(def env.exprs (ap field (symbol env) (symbol exprs)))
(def env.locals (ap field (symbol env) (symbol locals)))
(def env.globals (ap field (symbol env) (symbol globals)))
(def env.heap (ap field (symbol env) (symbol heap)))
(def env.def (ap field (symbol env) (symbol def)))
(def env.index (ap field (symbol env) (symbol index)))

;; Gets the variable with a name in an environment.
(def env.get
  (fn env'
    (fn name
      (ap with (ap tree-map.get (ap env.locals env') name)
        (fn option
          (if (ap some? option)
            option
            (ap tree-map.get (ap env.globals env') name)))))))

;; The result of generating an expr.
(def generate-result
  (ap new-data (symbol generate-result)
    (ap list3 (symbol operation) (symbol declarations) (symbol env))))

(def generate-result.operation
  (ap field (symbol generate-result) (symbol operation)))

(def generate-result.declarations
  (ap field (symbol generate-result) (symbol declarations)))

(def generate-result.env
  (ap field (symbol generate-result) (symbol env)))

;; Generates an identifier expression.
(def generate-identifier-expr
  (fn name
    (fn env'
      (ap with (ap env.get env' name)
        (fn option
        (if (ap some? option)
          (ap with (ap unnull option)
          (fn variable
            (if (ap global-variable? variable)
              (ap generate-result
                (ap global-variable-operation
                  (ap global-variable.name variable)
                  (ap global-variable.path variable))
                ()
                env')
              (ap generate-result
                (ap local-variable-operation
                  (ap local-variable.name variable)
                  (ap local-variable.index variable))
                ()
                env'))))
          (if (ap nil? (ap env.exprs env'))
            (ap error (ap concat (ap symbol->list (symbol "Could not find variable \""))
                      (ap concat name
                        (ap symbol->list (symbol "\"")))))
            (ap with
              (ap generate-expr
                (ap car (ap env.exprs env'))
                (ap env
                  (ap cdr (ap env.exprs env'))
                  (ap empty-tree-map compare-symbol)
                  (ap env.globals env')
                  (ap env.heap env')
                  ()
                  nat.0))
            (fn next
              (ap with
                (ap generate-identifier-expr
                  name
                  (ap env
                    (ap env.exprs (ap generate-result.env next))
                    (ap env.locals env')
                    (ap env.globals (ap generate-result.env next))
                    (ap env.heap env')
                    (ap env.def env')
                    (ap env.index env')))
              (fn result
                (ap generate-result
                  (ap combine-operation
                    (ap generate-result.operation next)
                    (ap generate-result.operation result))
                  (ap concat
                    (ap generate-result.declarations next)
                    (ap generate-result.declarations result))
                  (ap generate-result.env result)))))))))))))

;; Generates a nil expression.
(def generate-nil
  (fn env'
    (ap generate-result nil-operation () env')))

;; Generates an if expression.
(def generate-if-expr
  (fn cond-expr
    (fn true-expr
      (fn false-expr
        (fn env'
          (ap (fn cond-result
            (ap (fn true-result
              (ap (fn false-result
                (ap generate-result
                  (ap if-operation
                    (ap generate-result.operation cond-result)
                    (ap generate-result.operation true-result)
                    (ap generate-result.operation false-result))
                  (ap concat
                    (ap generate-result.declarations cond-result)
                    (ap concat
                      (ap generate-result.declarations true-result)
                      (ap generate-result.declarations false-result)))
                  (ap generate-result.env false-result)))
              (ap generate-expr false-expr (ap generate-result.env true-result))))
            (ap generate-expr true-expr (ap generate-result.env cond-result))))
          (ap generate-expr cond-expr env')))))))

;; Generates a fn expression.
(def generate-fn-expr
  (fn name
    (fn expr
      (fn env'
        (ap with (ap mangle-fn-name (ap env.def env') (ap env.index env'))
        (fn mangled-name
          (ap with
            (ap env
              (ap env.exprs env')
              (ap env.locals env')
              (ap env.globals env')
              (ap env.heap env')
              (ap env.def env')
              (ap nat.+ nat.1 (ap env.index env')))
          (fn new-env
            (ap with (ap map (ap tree-map->list (ap closures expr new-env)) first)
            (fn closures
              (ap with
                (ap generate-expr expr
                  (ap env
                    (ap env.exprs new-env)
                    (ap second
                      (ap fold
                        (ap append closures name)
                        (ap pair nat.0 (ap env.locals new-env))
                        (fn vars
                          (fn closure
                            (ap pair
                              (ap nat.+ nat.1 (ap first vars))
                              (ap tree-map.put
                                (ap second vars)
                                closure
                                (ap local-variable closure (ap first vars))))))))
                    (ap env.globals new-env)
                    (ap env.heap new-env)
                    mangled-name
                    (ap env.index new-env)))
              (fn result
                (ap with
                  (ap fn-declaration
                    mangled-name
                    (ap expr.path expr)
                    closures
                    (ap generate-result.operation result))
                (fn declaration
                  (ap generate-result
                    (ap fn-operation
                      (ap expr.path expr)
                        mangled-name
                        (ap map closures
                        (fn closure
                          (ap generate-result.operation
                            (ap generate-identifier-expr closure new-env)))))
                    (ap append (ap generate-result.declarations result) declaration)
                    (ap env
                      (ap env.exprs (ap generate-result.env result))
                      (ap env.locals new-env)
                      (ap env.globals (ap generate-result.env result))
                      (ap interpret-fn-declaration declaration
                        (ap env.heap (ap generate-result.env result)))
                      (ap env.def new-env)
                      (ap env.index new-env)))))))))))))))))

;; Generates a def expression.
(def generate-def-expr
  (fn name
    (fn expr
      (fn env'
        (if (ap some? (ap env.get env' name))
            (ap error (ap concat name
                      (ap symbol->list (symbol " has already been defined"))))
          (ap with
            (ap env
              (ap env.exprs env')
              (ap env.locals env')
              (ap tree-map.put
                (ap env.globals env')
                name
                (ap global-variable name (ap expr.path expr)))
              (ap env.heap env')
              (ap env.def env')
              (ap env.index env'))
          (fn new-env
            (ap with
              (ap generate-expr
                expr
                (ap env
                  (ap env.exprs new-env)
                  (ap env.locals new-env)
                  (ap env.globals new-env)
                  (ap env.heap new-env)
                  name
                  (ap env.index new-env)))
            (fn result
              (ap with
                (ap def-declaration
                  name
                  (ap expr.path expr)
                  (ap generate-result.operation result))
              (fn declaration
                (ap generate-result
                  (ap def-operation
                    name
                    (ap expr.path expr)
                    (ap generate-result.operation result))
                  (ap cons declaration (ap generate-result.declarations result))
                  (ap env
                    (ap env.exprs (ap generate-result.env result))
                    (ap env.locals (ap generate-result.env result))
                    (ap env.globals (ap generate-result.env result))
                    (ap interpret-def-declaration declaration
                      (ap env.heap (ap generate-result.env result)))
                    (ap env.def new-env)
                    (ap env.index (ap generate-result.env result)))))))))))))))

;; Generates a impure expression.
(def generate-impure-expr
  (fn expr
    (fn env'
      (ap with (ap generate-expr expr env')
      (fn result
        (ap generate-result
          (ap impure-operation (ap generate-result.operation result))
          (ap generate-result.declarations result)
          (ap generate-result.env result)))))))

;; Generates a macro expression.
(def generate-macro-expr
  (fn macro
    (fn exprs
      (fn env'
        (ap with (ap generate-expr macro env')
        (fn result
          (ap with
            (ap interpret-declarations
              (ap generate-result.declarations result)
              (ap env.heap env'))
          (fn new-heap
            (ap with
              (ap interpret-operation
                (ap generate-result.operation result)
                ()
                new-heap)
            (fn fn
              (ap generate-expr
                (ap list->expr (ap right (ap fn (ap map exprs expr->list))))
                (ap env
                  (ap env.exprs (ap generate-result.env result))
                  (ap env.locals env')
                  (ap env.globals (ap generate-result.env result))
                  new-heap
                  (ap env.index env')
                  (ap env.def env')))))))))))))

;; Generates a symbol expression.
(def generate-symbol-expr
  (fn name
    (fn env'
      (ap generate-result (ap symbol-operation name) () env'))))

;; Generates an apply expression.
(def generate-apply-expr
  (fn fn
    (fn args
      (fn env'
        (if (ap nil? args)
          (ap generate-apply-expr
            fn
            (ap cons
              (ap list-expr () (ap expr.path fn) (ap expr.start fn) (ap expr.end fn))
              ())
            env')
        (if (ap nil? (ap cdr args))
          (ap (fn fn-result
            (ap (fn arg-result
              (ap generate-result
                (ap apply-operation
                  (ap generate-result.operation fn-result)
                  (ap generate-result.operation arg-result))
                (ap concat
                  (ap generate-result.declarations fn-result)
                  (ap generate-result.declarations arg-result))
                (ap generate-result.env arg-result)))
            (ap generate-expr (ap car args) (ap generate-result.env fn-result))))
          (ap generate-expr fn env'))
          (ap generate-apply-expr
            (ap list-expr
              (ap list3
                (ap identifier-expr (symbol "ap") (ap expr.path fn)
                  (ap expr.start fn) (ap expr.end fn))
                fn
                (ap car args))
              (ap expr.path fn)
              (ap expr.start fn)
              (ap expr.end fn))
            (ap cdr args)
            env')))))))

;; Generates a list expression.
(def generate-list-expr
  (fn expr
    (fn env'
      (ap (fn exprs
        (if (ap nil? exprs)
          (ap generate-nil env')
          (ap (fn name
            (if (ap list.= char.= name (ap symbol->list (symbol if)))
              (ap generate-if-expr
                (ap cadr exprs)
                (ap caddr exprs)
                (ap cadddr exprs)
                env')
            (if (ap list.= char.= name (ap symbol->list (symbol fn)))
              (ap generate-fn-expr
                (ap identifier-expr.name (ap cadr exprs))
                (ap caddr exprs)
                env')
            (if (ap list.= char.= name (ap symbol->list (symbol def)))
              (ap generate-def-expr
                (ap identifier-expr.name (ap cadr exprs))
                (ap caddr exprs)
                env')
            (if (ap list.= char.= name (ap symbol->list (symbol impure)))
              (ap generate-impure-expr
                (ap cadr exprs)
                env')
            (if (ap list.= char.= name (ap symbol->list (symbol ap)))
              (ap generate-apply-expr
                (ap cadr exprs)
                (ap cddr exprs)
                env')
            (if (ap list.= char.= name (ap symbol->list (symbol symbol)))
              (ap generate-symbol-expr
                (ap identifier-expr.name (ap cadr exprs))
                env')
              (ap generate-macro-expr
                (ap car exprs)
                (ap cdr exprs)
                env'))))))))
          (if (ap identifier-expr? (ap car exprs))
            (ap identifier-expr.name (ap car exprs))
            ()))))
      (ap list-expr.exprs expr)))))

;; Generates a single expression.
(def generate-expr
  (fn expr
    (fn env'
      (ap (fn expr-result
        (ap generate-result
          (ap line-number-operation
            (ap generate-result.operation expr-result)
            (ap position.line (ap expr.start expr)))
          (ap generate-result.declarations expr-result)
          (ap generate-result.env expr-result)))
      (if (ap identifier-expr? expr)
        (ap generate-identifier-expr (ap identifier-expr.name expr) env')
        (ap generate-list-expr expr env'))))))

;; Generates an M environment.
(def generate-env
  (fn env'
    (if (ap nil? (ap env.exprs env'))
      (ap generate-result nil-operation () env')
      (ap with
        (ap generate-expr
          (ap car (ap env.exprs env'))
          (ap env
            (ap cdr (ap env.exprs env'))
            (ap env.locals env')
            (ap env.globals env')
            (ap env.heap env')
            (ap env.def env')
            (ap env.index env')))
      (fn car-result
        (ap with (ap generate-env (ap generate-result.env car-result))
        (fn cdr-result
          (ap generate-result
            (ap combine-operation
              (ap generate-result.operation car-result)
              (ap generate-result.operation cdr-result))
            (ap concat
              (ap generate-result.declarations car-result)
              (ap generate-result.declarations cdr-result))
            (ap generate-result.env cdr-result)))))))))