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

;; Gets the variable with a name in an environment.
(def env.get
  (fn env'
    (fn name
      (with (tree-map.get (env.locals env') name)
        (fn option
          (if (some? option)
            option
            (tree-map.get (env.globals env') name)))))))

;; True if a name has been defined as a macro.
(def env.macro?
  (fn env'
    (fn name
      (with (tree-map.get (env.globals env') name)
      (fn option
        (if (null? option)
          false
          (global-variable.macro? (unnull option))))))))