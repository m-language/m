;; The empty environment.
(def env/empty (const (pair (pair null null) false)))

;; Gets a value in an environment.
(defn env/get env name
  (env name))

;; Puts a definition in an environment.
(defn env/put env name value def macro?
  (fn n
    (if (symbol.= name n)
      (pair (pair value def) macro?)
      (env/get env n))))

;; Gets a value in an environment.
(defn env/get-value env name
  (first (first (env/get env name))))

;; Gets a definition in an environment.
(defn env/get-def env name
  (second (first (env/get env name))))

;; Tests if a definition is a macro.
(defn env/get-macro? env name
  (second (env/get env name)))