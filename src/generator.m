;; Mangles the name of a function given an index.
(def mangle-fn-name ())

;; The result of generating an expression with unresolved dependencies.
(def generating
  (new-data (symbol generating)
    (list3 (symbol dependencies) (symbol global-env) (symbol continue))))

(def generating.dependencies (field (symbol generating) (symbol dependencies)))
(def generating.global-env (field (symbol generating) (symbol global-env)))
(def generating.continue (field (symbol generating) (symbol continue)))
(def generating? (is? (symbol generating)))

;; The result of generating an expression without unresolved dependencies.
(def generated
  (new-data (symbol generated)
    (list3 (symbol operation) (symbol declarations) (symbol global-env))))

(def generated.operation (field (symbol generated) (symbol operation)))
(def generated.declarations (field (symbol generated) (symbol declarations)))
(def generated.global-env (field (symbol generated) (symbol global-env)))
(def generated? (is? (symbol generated)))

;; The result of generating an invalid expression.
(def degenerate
  (new-data (symbol degenerate)
    (list2 (symbol errors) (symbol global-env))))

(def degenerate.errors (field (symbol degenerate) (symbol errors)))
(def degenerate.global-env (field (symbol degenerate) (symbol global-env)))
(def degenerate? (is? (symbol degenerate)))

(def degenerate.with-global-env
  (fn env degenerate'
    (degenerate (degenerate.errors degenerate') env)))

;; Matches on a generate-result.
(def generate-result.match
  (fn result degenerate' generating' generated'
    (if (generated? result) (generated' result)
    (if (generating? result) (generating' result)
    (if (degenerate? result) (degenerate' result)
      (error (symbol "...")))))))

(def generate-result.global-env
  (fn result
    (generate-result.match result degenerate.global-env generating.global-env generated.global-env)))

;; Combines two generator results.
(def generate-result.combine
  (fn result1 result2 global-env f
    (generate-result.match result1
      (fn degenerate1 (degenerate.combine degenerate1 result2 global-env f))
      (fn generating1 (generating.combine generating1 result2 global-env f))
      (fn generated1 (generated.combine generated1 result2 global-env f)))))

;; Combines a degenerate with a generator result.
(def degenerate.combine
  (fn degenerate1 result global-env f
    (generate-result.match result
    (fn degenerate2
      (degenerate
        (concat (degenerate.errors degenerate1) (degenerate.errors degenerate2))
        global-env))
    (fn generating2 (degenerate.with-global-env global-env degenerate1))
    (fn generated2 (degenerate.with-global-env global-env degenerate1)))))

;; Combines a generating with a generator result.
(def generating.combine
  (fn generating1 result global-env f
    (generate-result.match result
      (fn degenerate2 (degenerate.with-global-env global-env degenerate2))
      (fn generating2
        (generating
          (concat (generating.dependencies generating1) (generating.dependencies generating2))
          global-env
          (fn global-env
            (generating.combine generating1 generating2 global-env f))))
      (fn generated2 (generated-resolve-generating generated.combine generated2 generating1 global-env (swap f))))))

;; Combines a generated with a generator result.
(def generated.combine
  (fn generated1 result global-env f
    (generate-result.match result
      (fn degenerate2 (degenerate.with-global-env global-env degenerate2))
      (fn generating2 (generated-resolve-generating generated.combine generated1 generating2 global-env f))
      (fn generated2
        (generated
          (f (generated.operation generated1) (generated.operation generated2))
          (concat (generated.declarations generated1) (generated.declarations generated2))
          global-env)))))

;; Resolves all dependencies of a generating given a generated.
(def generated-resolve-generating
  (fn generated.combine generated' generating' global-env f
    (generating (generating.dependencies generating') global-env
      (fn global-env
        (generated.combine generated'
          (generating.continue generating' global-env)
          global-env
          f)))))

;; The set of closures in an expression.
(def closures
  (fn local-env
    (closures' local-env (empty-tree-map compare-symbol))))

(def closures'
  (fn local-env acc expr
    (if (identifier-expr? expr)
      (if (null? (tree-map.get (local-env.locals local-env) (identifier-expr.name expr)))
        acc
        (tree-map.put acc (identifier-expr.name expr) true))
      (fold (list-expr.exprs expr) acc (closures' local-env)))))

;; Generates a global expression.
(def generate-global-expr
  (fn macro? generate-expr name value local-env global-env
    (if (some? (tree-map.get (global-env.globals global-env) name))
      (degenerate (list1 (concat name (symbol " has already been defined"))) global-env)
      (with
        ((swap global-env.with-globals) global-env
          (tree-map.put (global-env.globals global-env) name
            (global-variable name (expr.path value) macro?)))
      (fn new-global-env
        (generate-result.match (generate-expr value (local-env.with-def name local-env) new-global-env)
        (fn degenerate' degenerate')
        (fn generating'
          (generated
            (def-operation name (expr.path value) nil-operation)
            ()
            ((swap global-env.with-dependents) global-env
              (with (global-env.dependents (generating.global-env generating'))
              (fn dependents
                (with (car (generating.dependencies generating'))
                (fn dependency
                  (tree-map.put dependents dependency
                    (fn global-env
                      (if (null? (tree-map.get dependents dependency))
                        (generate-global-expr macro? generate-expr name value local-env global-env)
                        (with ((unnull (tree-map.get dependents dependency)) global-env)
                        (fn result1
                          (with
                            (generate-global-expr macro? generate-expr name value local-env
                              (generate-result.global-env result1))
                          (fn result2
                            (generate-result.combine result1 result2
                              (generate-result.global-env result2)
                              false)))))))))))))))
        (fn generated'
          (with (def-declaration name (expr.path value) (generated.operation generated'))
          (fn declaration
            (with (tree-map.get (global-env.dependents (generated.global-env generated')) name)
            (fn generating?
              (with
                (generated
                  (def-operation name (expr.path value) (generated.operation generated'))
                  (append (generated.declarations generated') declaration)
                  ((swap global-env.with-heap) (generated.global-env generated')
                    (interpret-def-declaration declaration (global-env.heap (generated.global-env generated')))))
              (fn result1
                (if (null? generating?) result1
                  (with ((unnull generating?) (generated.global-env result1))
                  (fn result2
                    (generated.combine result1 result2
                      (generate-result.global-env result2)
                      true)))))))))))))))))

;; Generates a def expression.
(def generate-def-expr (generate-global-expr false))

;; Generates a macro expression.
(def generate-macro-expr (generate-global-expr true))

;; Generates an identifier expression.
(def generate-identifier-expr
  (fn name local-env global-env
    (with (env.get local-env global-env name)
    (fn option
      (if (some? option)
        (generated (generate-identifier-expr' (unnull option)) () global-env)
        (generating (list1 name) global-env
          (fn global-env
            (generate-identifier-expr name local-env global-env))))))))

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
  (fn local-env global-env
    (generated nil-operation () global-env)))

;; Generates an if expression.
(def generate-if-expr
  (fn generate-expr cond-expr true-expr false-expr local-env global-env
    (generate-expr
      (list-expr
        (list2
          (list-expr
            (list3 cond-expr
              (list-expr
                (list3
                  (identifier-expr (symbol fn) (expr.path true-expr) (expr.start true-expr) (expr.end true-expr))
                  (identifier-expr (symbol "") (expr.path true-expr) (expr.start true-expr) (expr.end true-expr))
                  true-expr)
                (expr.path true-expr)
                (expr.start true-expr)
                (expr.end true-expr))
              (list-expr
                (list3
                  (identifier-expr (symbol fn) (expr.path false-expr) (expr.start false-expr) (expr.end false-expr))
                  (identifier-expr (symbol "") (expr.path false-expr) (expr.start false-expr) (expr.end false-expr))
                  false-expr)
                (expr.path false-expr)
                (expr.start false-expr)
                (expr.end false-expr)))
            (expr.path cond-expr)
            (expr.start cond-expr)
            (expr.end cond-expr))
          (list-expr () (expr.path cond-expr) (expr.start cond-expr) (expr.start cond-expr)))
        (expr.path cond-expr)
        (expr.start cond-expr)
        (expr.end cond-expr))
      local-env
      global-env)))

;; Generates a fn expression.
(def generate-fn-expr
  (fn generate-expr names value local-env global-env
    (if (nil? (cdr names))
      (generate-fn-expr' generate-expr (car names) value local-env global-env)
      (with
        (list-expr
          (list3
            (identifier-expr (symbol fn) (expr.path value) (expr.start value) (expr.end value))
            (identifier-expr (last names) (expr.path value) (expr.start value) (expr.end value))
            value)
          (expr.path value)
          (expr.start value)
          (expr.end value))
      (fn new-value
        (generate-fn-expr generate-expr (init names) new-value local-env global-env))))))

(def generate-fn-expr'
  (fn generate-expr name value local-env global-env
    (with (mangle-fn-name (local-env.def local-env) (global-env.index global-env))
    (fn mangled-name
      (with (global-env.with-index (nat.+ nat.1 (global-env.index global-env)) global-env)
      (fn new-global-env
        (with (map (tree-map->list (closures local-env value)) first)
        (fn closures
          (generate-result.match
            (generate-expr value
              (local-env.with-locals (generate-fn-expr-closures closures name local-env)
                (local-env.with-def mangled-name local-env))
              new-global-env)
          (fn degenerate' degenerate')
          (fn generating'
            (generating (generating.dependencies generating') (generating.global-env generating')
              (fn global-env
                (generate-fn-expr' name value local-env global-env))))
          (fn generated'
            (with (fn-declaration mangled-name (expr.path value) closures (generated.operation generated'))
            (fn declaration
              (generated
                (fn-operation
                  (expr.path value)
                  mangled-name
                  name
                  (generated.operation generated')
                  (map closures
                    (fn closure
                      (generate-identifier-expr'
                        (unnull (env.get local-env (generated.global-env generated') closure))))))
                (append (generated.declarations generated') declaration)
                ((swap global-env.with-heap) (generated.global-env generated')
                  (interpret-fn-declaration declaration
                    (global-env.heap (generated.global-env generated')))))))))))))))))

(def generate-fn-expr-closures
  (fn closures name local-env
    (second
      (fold (append closures name) (pair nat.0 (local-env.locals local-env))
        (fn vars closure
          (pair
            (nat.+ nat.1 (first vars))
            (tree-map.put (second vars) closure
              (local-variable closure (first vars)))))))))

;; Generates a symbol expression.
(def generate-symbol-expr
  (fn name local-env global-env
    (generated (symbol-operation name) () global-env)))

;; Generates an apply expression.
(def generate-apply-expr
  (fn generate-expr fn args local-env global-env
    (with (generate-expr fn local-env global-env)
    (fn fn-result
      (fold args fn-result
        (fn fn-result arg
          (with (generate-expr arg local-env (generate-result.global-env fn-result))
          (fn arg-result
            (generate-apply-expr' fn-result arg-result)))))))))

(def generate-apply-expr'
  (fn fn-result arg-result
    (generate-result.combine fn-result arg-result
      (generate-result.global-env arg-result)
      apply-operation)))

;; Generates an expression which may be a macro.
(def generate-macro?-expr
  (fn generate-expr expr fn args local-env global-env
    (with (identifier-expr.name fn)
    (fn name
      (with (env.get local-env global-env name)
      (fn option
        (if (null? option)
          (generating (list1 name) global-env
            (fn global-env
              (generate-macro?-expr expr fn args local-env global-env)))
          (with (unnull option)
          (fn variable
            (if (and (global-variable? variable)
                     (fn "" (global-variable.macro? variable)))
              (generate-macro-apply-expr generate-expr expr name args local-env global-env)
              (generate-apply-expr generate-expr fn args local-env global-env)))))))))))

;; Generates a macro application expression.
(def generate-macro-apply-expr
  (fn generate-expr expr name args local-env global-env
    (with (heap.get (global-env.heap global-env) name)
    (fn function
      (generate-expr
        (list-expr
          (map (function (map args expr->list)) list->expr)
          (identifier-expr.path expr)
          (identifier-expr.start expr)
          (identifier-expr.end expr))
        local-env
        global-env)))))

;; Generates a list expression.
(def generate-list-expr
  (fn generate-expr expr local-env global-env
    (with (list-expr.exprs expr)
    (fn exprs
      (if (nil? exprs)
        (generate-nil local-env global-env)
        (if (identifier-expr? (car exprs))
          (with (identifier-expr.name (car exprs))
          (fn name
            (if (list.= char.= name (symbol->list (symbol if)))
              (generate-if-expr generate-expr
                (cadr exprs)
                (caddr exprs)
                (cadddr exprs)
                local-env global-env)
            (if (list.= char.= name (symbol->list (symbol fn)))
              (generate-fn-expr generate-expr
                (map (init (cdr exprs)) identifier-expr.name)
                (last exprs)
                local-env global-env)
            (if (list.= char.= name (symbol->list (symbol def)))
              (generate-def-expr generate-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                local-env global-env)
            (if (list.= char.= name (symbol->list (symbol macro)))
              (generate-macro-expr generate-expr
                (identifier-expr.name (cadr exprs))
                (caddr exprs)
                local-env global-env)
            (if (list.= char.= name (symbol->list (symbol symbol)))
              (generate-symbol-expr
                (identifier-expr.name (cadr exprs))
                local-env global-env)
              (generate-macro?-expr generate-expr
                expr
                (car exprs)
                (cdr exprs)
                local-env global-env))))))))
          (generate-apply-expr generate-expr
            (car exprs)
            (cdr exprs)
            local-env global-env)))))))

;; Generates an expression.
(def generate-expr
  (fn expr local-env global-env
    (generate-result.match
      (if (identifier-expr? expr)
        (generate-identifier-expr (identifier-expr.name expr) local-env global-env)
        (generate-list-expr generate-expr expr local-env global-env))
    (fn degenerate' degenerate')
    (fn generating' generating')
    (fn generated'
      (generated
        (line-number-operation (generated.operation generated')
          (position.line (expr.start expr)))
        (generated.declarations generated')
        (generated.global-env generated'))))))

;; Generates a list of expressions.
(def generate-exprs
  (fn exprs global-env
    (generate-exprs' exprs (generated nil-operation () global-env))))

(def generate-exprs'
  (fn exprs result
    (if (nil? exprs) result
    (with (generate-expr (car exprs) default-local-env (generate-result.global-env result))
    (fn car-result
      (generate-exprs' (cdr exprs)
        (generate-result.combine result car-result
          (generate-result.global-env car-result)
          true)))))))