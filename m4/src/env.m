(import predef)

;; The environment of a variable.
(def env
  (lambda vars
    (lambda path
      (lambda def
        (lambda index
          (lambda imports
            (derive (symbol env) (symbol vars) vars
            (derive (symbol env) (symbol path) path
            (derive (symbol env) (symbol def) def
            (derive (symbol env) (symbol index) index
            (derive (symbol env) (symbol imports) imports
              (object (symbol env)))))))))))))

(def env.vars (field (symbol env) (symbol vars)))
(def env.path (field (symbol env) (symbol path)))
(def env.def (field (symbol env) (symbol def)))
(def env.index (field (symbol env) (symbol index)))
(def env.imports (field (symbol env) (symbol imports)))