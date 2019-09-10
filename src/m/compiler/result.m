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
(defnrec generating.combine generating1 result global-env f
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
(defnrec generated.combine generated1 result global-env f
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
  (generated operation nil
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
