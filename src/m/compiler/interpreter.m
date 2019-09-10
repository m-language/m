;; The empty ienv.
(def ienv/nil (const null))

;; Adds a value to an ienv.
(defn ienv/cons env name value
  (fn x
    (if (symbol.= x name) (some value) (env x))))

;; Concatenates two ienvs.
(defn ienv/concat env env'
  (fn x
    (let value? (env x)
      (if (null? value?)
        (env' x)
        value?))))

;; Interpets a tree's definitions.
(defnrec interpret-env tree env
  (let type (type-name tree)
       f (pcond (symbol.= type)
           (symbol tree/val) interpret-env-val
           (symbol tree/def) interpret-env-def
           (symbol tree/fn) interpret-env-fn
           (symbol tree/ap) interpret-env-ap
           (symbol tree/symbol) interpret-env-symbol
           (error (symbol "...")))
    (f interpret-env tree env)))

(defn interpret-env-val interpret-env tree/val env
  env)

(defn interpret-env-def interpret-env tree/def env
  (ienv/cons env (tree/def.name tree/def)
    (interpret (tree/def.value tree/def) env)))

(defn interpret-env-fn interpret-env tree/fn env
  (interpret-env (tree/fn.value tree/fn) env))

(defn interpret-env-ap interpret-env tree/ap env
  (ienv/concat
    (interpret-env (tree/ap.fn tree/ap) env)
    (interpret-env (tree/ap.arg tree/ap) env)))

(defn interpret-env-symbol interpret-env tree/symbol env
  env)

;; Interprets a tree.
(defnrec interpret tree env
  (let type (type-name tree)
       f (pcond (symbol.= type)
           (symbol tree/val) interpret-val
           (symbol tree/def) interpret-def
           (symbol tree/fn) interpret-fn
           (symbol tree/ap) interpret-ap
           (symbol tree/symbol) interpret-symbol
           (error (symbol "...")))
    (f interpret tree env)))

(defn interpret-val interpret tree/val env
  (heap.get env (tree/val.name tree/val)))

(defn interpret-def interpret tree/def env
  (heap.get env (tree/def.name tree/def)))

(defn interpret-fn interpret tree/fn env
  (fn arg
    (interpret (tree/fn.value tree/fn)
      (ienv/cons (tree/fn.arg tree/fn) arg))))

(defn interpret-ap interpret tree/ap env
  ((interpret (tree/ap.fn tree/ap) env)
    (interpret (tree/ap.arg tree/ap) env)))

(defn interpret-symbol interpret tree/symbol env
  (tree/symbol.name tree/symbol env))
