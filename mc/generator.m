;; Mangles the name of a function given an index.
(extern mangle-fn-name)

;; The result of generating an expression with unresolved dependencies.
(def generating
  (new-data (symbol generating)
    (list (symbol dependencies) (symbol global-env) (symbol continue))))

(def generating.dependencies (field (symbol generating) (symbol dependencies)))
(def generating.global-env (field (symbol generating) (symbol global-env)))
(def generating.continue (field (symbol generating) (symbol continue)))
(def generating? (is? (symbol generating)))

;; The result of generating an expression without unresolved dependencies.
(def generated
  (new-data (symbol generated)
    (list (symbol operation) (symbol declarations) (symbol global-env))))

(def generated.operation (field (symbol generated) (symbol operation)))
(def generated.declarations (field (symbol generated) (symbol declarations)))
(def generated.global-env (field (symbol generated) (symbol global-env)))
(def generated? (is? (symbol generated)))

;; The result of generating an invalid expression.
(def degenerate
  (new-data (symbol degenerate)
    (list (symbol errors) (symbol global-env))))

(def degenerate.errors (field (symbol degenerate) (symbol errors)))
(def degenerate.global-env (field (symbol degenerate) (symbol global-env)))
(def degenerate? (is? (symbol degenerate)))

(def degenerate.with-global-env
  (fn env degenerate'
    (degenerate (degenerate.errors degenerate') env)))

;; Matches on a generate-result.
(defn generate-result.match result degenerate' generating' generated'
  (if (generated? result) (generated' result)
  (if (generating? result) (generating' result)
  (if (degenerate? result) (degenerate' result)
    (error (symbol "..."))))))

(defn generate-result.global-env result
  (generate-result.match result degenerate.global-env generating.global-env generated.global-env))

;; Combines two generator results.
(defn generate-result.combine result1 result2 global-env f
  (generate-result.match result1
    (fn degenerate1 (degenerate.combine degenerate1 result2 global-env f))
    (fn generating1 (generating.combine generating1 result2 global-env f))
    (fn generated1 (generated.combine generated1 result2 global-env f))))

;; Combines a degenerate with a generator result.
(defn degenerate.combine degenerate1 result global-env f
  (generate-result.match result
  (fn degenerate2
    (degenerate
      (concat (degenerate.errors degenerate1) (degenerate.errors degenerate2))
      global-env))
  (fn generating2 (degenerate.with-global-env global-env degenerate1))
  (fn generated2 (degenerate.with-global-env global-env degenerate1))))

;; Combines a generating with a generator result.
(defn generating.combine generating1 result global-env f
  (generate-result.match result
    (fn degenerate2 (degenerate.with-global-env global-env degenerate2))
    (fn generating2
      (generating
        (concat (generating.dependencies generating1) (generating.dependencies generating2))
        global-env
        (fn global-env
          (generating.combine generating1 generating2 global-env f))))
    (fn generated2 (generated-resolve-generating generated.combine generated2 generating1 global-env (swap f)))))

;; Combines a generated with a generator result.
(defn generated.combine generated1 result global-env f
  (generate-result.match result
    (fn degenerate2 (degenerate.with-global-env global-env degenerate2))
    (fn generating2 (generated-resolve-generating generated.combine generated1 generating2 global-env f))
    (fn generated2
      (generated
        (f (generated.operation generated1) (generated.operation generated2))
        (concat (generated.declarations generated1) (generated.declarations generated2))
        global-env))))

;; Resolves all dependencies of a generating given a generated.
(defn generated-resolve-generating generated.combine generated' generating' global-env f
  (generating (generating.dependencies generating') global-env
    (fn global-env
      (generated.combine generated'
        (generating.continue generating' global-env)
        global-env
        f))))

;; Converts a generating to a generated.
(defn generating->generated generating' operation global-env continue
  (generated operation ()
    ((swap global-env.with-dependents) global-env
      (let dependents (global-env.dependents (generating.global-env generating'))
           dependency (car (generating.dependencies generating'))
        (tree-map.put dependents dependency
          (fn global-env
            (if (null? (tree-map.get dependents dependency))
              (continue global-env)
              (let result1 ((unnull (tree-map.get dependents dependency)) global-env)
                   result2 (continue (generate-result.global-env result1))
                (generate-result.combine result1 result2
                  (generate-result.global-env result2)
                  false)))))))))

;; The set of closures in an expression.
(defn closures local-env
  (closures' local-env (empty-tree-map compare-symbol)))

(defn closures' local-env acc expr
  (if (symbol-expr? expr)
    (if (null? (tree-map.get (local-env.locals local-env) (symbol-expr.name expr)))
      acc
      (tree-map.put acc (symbol-expr.name expr) true))
    (fold (list-expr.exprs expr) acc (closures' local-env))))

;; Generates a global expression.
(defn generate-global-expr macro? generate-expr name value local-env global-env
  (if (some? (tree-map.get (global-env.globals global-env) name))
    (degenerate (list (concat name (symbol " has already been defined"))) global-env)
    (let new-global-env
      ((swap global-env.with-globals) global-env
        (tree-map.put (global-env.globals global-env) name
          (global-variable name (expr.path value) macro?)))
      (generate-result.match (generate-expr value (local-env.with-def name local-env) new-global-env)
      (fn degenerate' degenerate')
      (fn generating'
        (generating->generated generating'
          (def-operation name (expr.path value) nil-operation)
          global-env
          (fn global-env
            (generate-global-expr macro? generate-expr name value local-env global-env))))
      (fn generated'
        (let declaration (def-declaration name (expr.path value) (generated.operation generated'))
             generating? (tree-map.get (global-env.dependents (generated.global-env generated')) name)
             result1 (generate-global-expr' name value declaration generated')
             result2 ((unnull generating?) (generated.global-env result1))
          (if (null? generating?) result1
            (generated.combine result1 result2
              (generate-result.global-env result2)
              true))))))))

(defn generate-global-expr' name value declaration generated'
  (generated
    (def-operation name (expr.path value) (generated.operation generated'))
    (append (generated.declarations generated') declaration)
    ((swap global-env.with-heap) (generated.global-env generated')
      (interpret-def-declaration declaration (global-env.heap (generated.global-env generated'))))))

;; Generates a def expression.
(def generate-def-expr (generate-global-expr false))

;; Generates a macro expression.
(def generate-macro-expr (generate-global-expr true))

;; Generates a symbol expression.
(defn generate-symbol-expr name local-env global-env
  (let option (env.get local-env global-env name)
    (if (some? option)
      (generated (generate-symbol-expr' (unnull option)) () global-env)
      (generating (list name) global-env
        (fn global-env
          (generate-symbol-expr name local-env global-env))))))

(defn generate-symbol-expr' variable
  (if (global-variable? variable)
    (global-variable-operation
      (global-variable.name variable)
      (global-variable.path variable))
    (local-variable-operation
      (local-variable.name variable)
      (local-variable.index variable))))

;; Generates a nil expression.
(defn generate-nil local-env global-env
  (generated nil-operation () global-env))

;; Generates a fn expression.
(defn generate-fn-expr generate-expr names value local-env global-env
  (if (nil? (cdr names))
    (generate-fn-expr' generate-expr (car names) value local-env global-env)
    (let new-value
      (list-expr
        (list
          (symbol-expr (symbol fn) (expr.path value) (expr.start value) (expr.end value))
          (symbol-expr (last names) (expr.path value) (expr.start value) (expr.end value))
          value)
        (expr.path value)
        (expr.start value)
        (expr.end value))
      (generate-fn-expr generate-expr (init names) new-value local-env global-env))))

(defn generate-fn-expr' generate-expr name value local-env global-env
  (let mangled-name (mangle-fn-name (local-env.def local-env) (global-env.index global-env))
       new-global-env (global-env.with-index (nat.+ nat.1 (global-env.index global-env)) global-env)
       closures (map (tree-map->list (closures local-env value)) first)
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
      (let declaration (fn-declaration mangled-name (expr.path value) closures (generated.operation generated'))
        (generated
          (fn-operation
            (expr.path value)
            mangled-name
            name
            (generated.operation generated')
            (map closures
              (fn closure
                (generate-symbol-expr'
                  (unnull (env.get local-env (generated.global-env generated') closure))))))
          (append (generated.declarations generated') declaration)
          ((swap global-env.with-heap) (generated.global-env generated')
            (interpret-fn-declaration declaration
              (global-env.heap (generated.global-env generated'))))))))))

(defn generate-fn-expr-closures closures name local-env
  (second
    (fold (append closures name) (pair nat.0 (local-env.locals local-env))
      (fn vars closure
        (pair
          (nat.+ nat.1 (first vars))
          (tree-map.put (second vars) closure
            (local-variable closure (first vars))))))))

;; Generates a symbol literal expression.
(defn generate-symbol-literal-expr name local-env global-env
  (generated (symbol-operation name) () global-env))

;; Generates an apply expression.
(defn generate-apply-expr generate-expr fn args local-env global-env
  (let fn-result (generate-expr fn local-env global-env)
    (fold args fn-result
      (fn fn-result arg
        (let arg-result (generate-expr arg local-env (generate-result.global-env fn-result))
          (generate-apply-expr' fn-result arg-result))))))

(defn generate-apply-expr' fn-result arg-result
  (generate-result.combine fn-result arg-result
    (generate-result.global-env arg-result)
    apply-operation))

;; Generates an expression which may be a macro.
(defn generate-macro?-expr generate-expr expr fn args local-env global-env
  (let name (symbol-expr.name fn)
       option (env.get local-env global-env name)
    (if (null? option)
      (generating (list name) global-env
        (fn global-env
          (generate-macro?-expr expr fn args local-env global-env)))
      (let variable (unnull option)
        (if (& (global-variable? variable) (global-variable.macro? variable))
          (generate-macro-apply-expr generate-expr expr name args local-env global-env)
          (generate-apply-expr generate-expr fn args local-env global-env))))))

;; Generates a macro application expression.
(defn generate-macro-apply-expr generate-expr expr name args local-env global-env
  (let function (heap.get (global-env.heap global-env) name)
       env (global-env->env global-env)
       result (function env args)
    (result/match result
      (fn new-expr
        (generate-expr
          (expr.with-path (expr.path expr) new-expr)
          local-env
          global-env))
      (fn errors
        (degenerate errors global-env))
      (fn dependencies
        (generating dependencies global-env
          (fn global-env
            (generate-macro-apply-expr generate-expr expr name args local-env global-env)))))))

;; Generates a list expression.
(defn generate-list-expr generate-expr expr local-env global-env
  (let exprs (list-expr.exprs expr)
    (if (nil? exprs)
      (generate-nil local-env global-env)
      (if (symbol-expr? (car exprs))
        (let name (symbol-expr.name (car exprs))
          (cond-satisfy (compose (list.= char.= name) symbol->list)
            (symbol fn)
              (generate-fn-expr generate-expr
                (map (init (cdr exprs)) symbol-expr.name)
                (last exprs)
                local-env global-env)
            (symbol def)
              (generate-def-expr generate-expr
                (symbol-expr.name (cadr exprs))
                (caddr exprs)
                local-env global-env)
            (symbol macro)
              (generate-macro-expr generate-expr
                (symbol-expr.name (cadr exprs))
                (caddr exprs)
                local-env global-env)
            (symbol symbol)
              (generate-symbol-literal-expr
                (symbol-expr.name (cadr exprs))
                local-env global-env)
            (generate-macro?-expr generate-expr
              expr
              (car exprs)
              (cdr exprs)
              local-env global-env)))
        (generate-apply-expr generate-expr
          (car exprs)
          (cdr exprs)
          local-env global-env)))))

;; Generates an expression.
(defn generate-expr expr local-env global-env
  (generate-result.match
    (if (symbol-expr? expr)
      (generate-symbol-expr (symbol-expr.name expr) local-env global-env)
      (generate-list-expr generate-expr expr local-env global-env))
  (fn degenerate' degenerate')
  (fn generating' generating')
  (fn generated'
    (generated
      (line-number-operation (generated.operation generated')
        (position.line (expr.start expr)))
      (generated.declarations generated')
      (generated.global-env generated')))))

;; Generates a list of expressions.
(defn generate-exprs exprs global-env
  (generate-exprs' exprs (generated nil-operation () global-env)))

(defn generate-exprs' exprs result
  (if (nil? exprs) result
  (let car-result
    (generate-result.match
      (generate-expr (car exprs) default-local-env (generate-result.global-env result))
      (fn degenerate' degenerate')
      (fn generating'
        (generating->generated generating' nil-operation
          (generate-result.global-env result)
          (fn global-env
            (generate-expr (car exprs) default-local-env global-env))))
      (fn generated' generated'))
    (generate-exprs' (cdr exprs)
      (generate-result.combine result car-result
        (generate-result.global-env car-result)
        true)))))
