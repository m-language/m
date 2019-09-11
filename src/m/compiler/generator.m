;; Mangles the name of a function given an index.
(extern mangle-fn-name)

;; The set of closures in an expression.
(defn closures local-env
  (closures' local-env (empty-tree-map compare-symbol)))

(defnrec closures' local-env acc expr
  (expr.match expr
    (fn name _
      (if (null? (tree-map.get (local-env.locals local-env) name))
        acc
        (tree-map.put acc name true)))
    (fn exprs _
      (fold exprs acc (closures' local-env)))))

;; Generates a global expression.
(defnrec generate-global-expr macro? generate-expr name value local-env global-env
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
      (interpret-declaration declaration (global-env.heap (generated.global-env generated'))))))

;; Generates a def expression.
(def generate-def-expr (generate-global-expr false))

;; Generates a macro expression.
(def generate-macro-expr (generate-global-expr true))

;; Generates a symbol expression.
(defnrec generate-symbol-expr name local-env global-env
  (let option (env.get local-env global-env name)
    (if (some? option)
      (generated (generate-symbol-expr' (unnull option)) nil global-env)
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

;; Generates a fn expression.
(defnrec generate-fn-expr generate-expr names value local-env global-env
  (if (nil? (cdr names))
    (generate-fn-expr' generate-expr (car names) value local-env global-env)
    (let new-value
      (list-expr
        (list
          (symbol-expr (symbol fn) (location (expr.path value) (span (expr.start value) (expr.end value))))
          (symbol-expr (last names) (location (expr.path value) (span (expr.start value) (expr.end value))))
          value)
        (location (expr.path value) (span (expr.start value) (expr.end value))))
      (generate-fn-expr generate-expr (init names) new-value local-env global-env))))

(defnrec generate-fn-expr' generate-expr name value local-env global-env
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
            (interpret-declaration declaration
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
  (generated (symbol-operation name) nil global-env))

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
(defnrec generate-macro?-expr generate-expr name fn args local-env global-env
  (let option (env.get local-env global-env name)
    (if (null? option)
      (generating (list name) global-env
        (fn global-env
          (generate-macro?-expr generate-expr fn args local-env global-env)))
      (let variable (unnull option)
        (if (& (global-variable? variable) (global-variable.macro? variable))
          (generate-macro-apply-expr generate-expr name fn args local-env global-env)
          (generate-apply-expr generate-expr fn args local-env global-env))))))

;; Generates a macro application expression.
(defnrec generate-macro-apply-expr generate-expr name fn args local-env global-env
  (let function (heap/get (global-env.heap global-env) name)
       env (global-env->env global-env)
       result (function env args)
    (result/match result
      (fn new-expr
        (generate-expr
          (expr.with-path (expr.path fn) new-expr)
          local-env
          global-env))
      (fn errors
        (degenerate errors global-env))
      (fn dependencies
        (generating dependencies global-env
          (fn global-env
            (generate-macro-apply-expr generate-expr fn args local-env global-env)))))))

;; Generates a list expression.
(defn generate-list-expr generate-expr exprs local-env global-env
  (if (nil? exprs)
    (degenerate (list (symbol "List of expressions is empty.")) global-env)
    (expr.match (car exprs)
      (fn name _
        (pcond (list.= char.= name)
          (symbol fn)
            (match-fn-expr (cdr exprs)
              (degenerate (list (symbol "Function has no expression.")) global-env)
              (degenerate (list (symbol "Function has no arguments.")) global-env)
              (fn expr (degenerate (list (symbol "Function argument is not a symbol.")) global-env))
              (fn args expr (generate-fn-expr generate-expr args expr local-env global-env)))
          (symbol def)
            (match-def-expr (cdr exprs)
              (degenerate (list (symbol "Definition has no name.")) global-env)
              (degenerate (list (symbol "Definition has no expression.")) global-env)
              (degenerate (list (symbol "Definition has extra expressions.")) global-env)
              (fn name (degenerate (list (symbol "Definition name is not a symbol.")) global-env))
              (fn name expr (generate-def-expr generate-expr name expr local-env global-env)))
          (symbol macro)
            (match-macro-expr (cdr exprs)
              (degenerate (list (symbol "Macro has no name.")) global-env)
              (degenerate (list (symbol "Macro has no expression.")) global-env)
              (degenerate (list (symbol "Macro has extra expressions.")) global-env)
              (fn name (degenerate (list (symbol "Macro name is not a symbol.")) global-env))
              (fn name expr (generate-macro-expr generate-expr name expr local-env global-env)))
          (symbol symbol)
            (match-symbol-literal-expr (cdr exprs)
              (degenerate (list (symbol "Symbol literal has no symbol.")) global-env)
              (degenerate (list (symbol "Symbol literal has extra expressions.")) global-env)
              (fn name (degenerate (list (symbol "Symbol literal is not a symbol.")) global-env))
              (fn name (generate-symbol-literal-expr name local-env global-env)))
          (generate-macro?-expr generate-expr name (car exprs) (cdr exprs) local-env global-env)))
      (fn _ _
        (match-apply-expr exprs
          (degenerate (list (symbol "Application has no arguments.")) global-env)
          (generate-apply-expr generate-expr (car exprs) (cdr exprs) local-env global-env))))))

;; Generates an expression.
(defnrec generate-expr expr local-env global-env
  (generate-result.match
    (expr.match expr
      (fn name _ (generate-symbol-expr name local-env global-env))
      (fn exprs _ (generate-list-expr generate-expr exprs local-env global-env)))
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
  (generate-exprs' exprs (generated nil-operation nil global-env)))

(defnrec generate-exprs' exprs result
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
