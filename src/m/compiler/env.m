;; The local environment.
(def local-env
  (new-data (symbol local-env)
    (list (symbol locals) (symbol def))))

(def local-env.locals (field (symbol local-env) (symbol locals)))
(def local-env.def (field (symbol local-env) (symbol def)))

(def local-env.with-locals
  (fn locals e
    (local-env locals (local-env.def e))))

(def local-env.with-def
  (fn def e
    (local-env (local-env.locals e) def)))

;; The global environment.
(def global-env
  (new-data (symbol global-env)
    (list (symbol globals) (symbol heap) (symbol dependents) (symbol index))))

(def global-env.globals (field (symbol global-env) (symbol globals)))
(def global-env.heap (field (symbol global-env) (symbol heap)))
(def global-env.dependents (field (symbol global-env) (symbol dependents)))
(def global-env.index (field (symbol global-env) (symbol index)))

(def global-env.with-globals
  (fn globals e
    (global-env globals (global-env.heap e) (global-env.dependents e) (global-env.index e))))

(def global-env.with-heap
  (fn heap e
    (global-env (global-env.globals e) heap (global-env.dependents e) (global-env.index e))))

(def global-env.with-dependents
  (fn dependents e
    (global-env (global-env.globals e) (global-env.heap e) dependents (global-env.index e))))

(def global-env.with-index
  (fn index e
    (global-env (global-env.globals e) (global-env.heap e) (global-env.dependents e) index)))

;; A list of unresolved variables.
(defn global-env.unresolved global-env
  (filter
    (map (tree-map->list (global-env.dependents global-env)) first)
    (fn dependent
      (null? (tree-map.get (global-env.globals global-env) dependent)))))

;; The default global M environment.
(def default-global-env
  (global-env (empty-tree-map compare-symbol) heap/nil (empty-tree-map compare-symbol) nat.0))

;; The default local M environment.
(def default-local-env
  (local-env (empty-tree-map compare-symbol) nil))

;; Gets the variable with a name in an environment.
(defn env.get local-env global-env name
  (let option (tree-map.get (local-env.locals local-env) name)
    (if (some? option)
      option
      (tree-map.get (global-env.globals global-env) name))))

;; Converts a global env to a macro env.
(defn global-env->env global-env
  (fn name
    (pair
      (pair
        ((global-env.heap global-env) name)
        null)
      ((tree-map.get (global-env.globals global-env) name)
        (const null)
        global-variable.macro?))))