;; Mangles the name of a function given an index.
(def mangle-fn-name ())

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

;; Generates a global expression.
(def generate-global-expr
  (fn macro?
    (fn name
      (fn expr
        (fn env'
          (if (some? (env.get env' name))
            (error (concat name (symbol " has already been defined")))
            (with
              (env
                (env.exprs env')
                (env.locals env')
                (tree-map.put
                  (env.globals env')
                  name
                  (global-variable name (expr.path expr) macro?))
                (env.heap env')
                (env.def env')
                (env.index env'))
            (fn new-env
              (run-with
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
                      (env.index (generate-result.env result))))))))))))))))

;; Generates an identifier expression.
(def generate-identifier-expr
  (fn name
    (fn env'
      (with (env.get env' name)
        (fn option
        (if (some? option)
          (with (unnull option)
          (fn variable
            (impure
              (generate-result (generate-identifier-expr' variable) () env'))))
          (if (nil? (env.exprs env'))
            (then-run-with (mpm-get-ref name)
            (fn ref
              (then-run-with (mpm-get-src ref)
              (fn src
                (with (parse src ref start-position ())
                (fn exprs
                  (generate-identifier-expr name
                    (env
                      exprs
                      (env.locals env')
                      (env.globals env')
                      (env.heap env')
                      (env.def env')
                      (env.index env')))))))))
            (then-run-with
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
              (run-with
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
                  (generate-result.operation result)
                  (concat
                    (generate-result.declarations next)
                    (generate-result.declarations result))
                  (generate-result.env result)))))))))))))

(def generate-identifier-expr'
  (fn variable
    (if (global-variable? variable)
      (global-variable-operation
        (global-variable.name variable)
        (global-variable.path variable))
      (local-variable-operation
        (local-variable.name variable)
        (local-variable.index variable)))))

;; Generates a nil expression.
(def generate-nil
  (fn env'
    (impure (generate-result nil-operation () env'))))

;; Generates an if expression.
(def generate-if-expr
  (fn cond-expr
    (fn true-expr
      (fn false-expr
        (fn env'
          (then-run-with (generate-expr cond-expr env')
          (fn cond-result
            (then-run-with (generate-expr true-expr (generate-result.env cond-result))
            (fn true-result
              (run-with (generate-expr false-expr (generate-result.env true-result))
              (fn false-result
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
                  (generate-result.env false-result)))))))))))))

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
              (run-with
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
                      name
                      (generate-result.operation result)
                      (map closures
                        (fn closure
                          (generate-identifier-expr' (unnull (env.get new-env closure))))))
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
  (generate-global-expr false))

;; Generates a macro expression.
(def generate-macro-expr
  (generate-global-expr true))

;; Generates a symbol expression.
(def generate-symbol-expr
  (fn name
    (fn env'
      (impure (generate-result (symbol-operation name) () env')))))

;; Generates an apply expression.
(def generate-apply-expr
  (fn fn
    (fn args
      (fn env'
        (then-run-with (generate-expr fn env')
        (fn fn-result
          (if (and (identifier-expr? fn)
                   (fn ""
                     (env.macro?
                       (generate-result.env fn-result)
                       (identifier-expr.name fn))))
            (with
              (interpret-operation
                (generate-result.operation fn-result)
                (env.heap (generate-result.env fn-result)))
            (fn function
              (generate-expr
                (list-expr
                  (map (function (map args expr->list)) list->expr)
                  (identifier-expr.path fn)
                  (identifier-expr.start fn)
                  (identifier-expr.end fn))
                (generate-result.env fn-result))))
            (fold args (impure fn-result)
              (fn proc
                (fn arg
                  (then-run-with proc
                  (fn fn-result
                    (run-with (generate-expr arg (generate-result.env fn-result))
                    (fn arg-result
                      (generate-apply-expr' fn-result arg-result)))))))))))))))

(def generate-apply-expr'
  (fn fn-result
    (fn arg-result
      (generate-result
        (apply-operation
          (generate-result.operation fn-result)
          (generate-result.operation arg-result))
        (concat
          (generate-result.declarations fn-result)
          (generate-result.declarations arg-result))
        (generate-result.env arg-result)))))

;; Generates a list expression.
(def generate-list-expr
  (fn expr
    (fn env'
      (with (list-expr.exprs expr)
      (fn exprs
        (if (nil? exprs)
          (generate-nil env')
          (with
            (if (identifier-expr? (car exprs))
              (identifier-expr.name (car exprs))
              ())
          (fn name
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
            (if (list.= char.= name (symbol->list (symbol macro)))
              (generate-macro-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                env')
            (if (list.= char.= name (symbol->list (symbol symbol)))
              (generate-symbol-expr
                (identifier-expr.name (cadr exprs))
                env')
              (generate-apply-expr
                (car exprs)
                (cdr exprs)
                env'))))))))))))))

;; Generates a single expression.
(def generate-expr
  (fn expr
    (fn env'
      (run-with
        (if (identifier-expr? expr)
          (generate-identifier-expr (identifier-expr.name expr) env')
          (generate-list-expr expr env'))
      (fn expr-result
        (generate-result
          (line-number-operation
            (generate-result.operation expr-result)
            (position.line (expr.start expr)))
          (generate-result.declarations expr-result)
          (generate-result.env expr-result)))))))

;; Generates an M environment.
(def generate-env
  (fn env'
    (if (nil? (env.exprs env'))
      (impure (generate-result nil-operation () env'))
      (then-run-with
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
        (run-with (generate-env (generate-result.env car-result))
        (fn cdr-result
          (generate-result
            (generate-result.operation cdr-result)
            (concat
              (generate-result.declarations car-result)
              (generate-result.declarations cdr-result))
            (generate-result.env cdr-result)))))))))