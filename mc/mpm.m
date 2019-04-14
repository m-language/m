;; The root file of the local mpm storage.
(extern mpm-root)

;; The root file of the local mpm reference storage.
(def mpm-ref-root (file.child mpm-root (symbol ref)))

;; The root file of the local mpm source storage.
(def mpm-src-root (file.child mpm-root (symbol src)))

;; Replaces all special symbols.
(def normalize id)

;; Retrieves all special symbols
(def unnormalize id)

;; Gets the reference file in mpm-root given a name.
(defn mpm-get-ref name
  (file.child mpm-ref-root (normalize name)))

;; Gets the source file in mpm-root given a reference.
(defn mpm-get-src ref
  (file.child mpm-src-root (concat ref (symbol ".m"))))

;; Puts all declarations in mpm-root as references.
(defn mpm-put-refs declarations
  (let def-declarations (filter declarations def-declaration?)
    (fold def-declarations (impure ())
      (fn process declaration
        (then-run process
          (file.write
            (file.child mpm-ref-root
              (normalize (def-declaration.name declaration)))
            (def-declaration.path declaration)))))))

;; Puts all sources in mpm-root.
(defn mpm-put-srcs in
  (file.copy in mpm-src-root))

;; Puts a file in mpm.
(defn mpm-put in
  (then-run-with (generate in)
  (fn result
    (then-run
      (mpm-put-refs (generated.declarations result))
      (mpm-put-srcs in)))))

;; Resolves a generate result with mpm.
(defn mpm-resolve-generate-result result
  ((swap run-with) second
    (mpm-resolve-generate-result' (empty-tree-map compare-symbol) result)))

(defn mpm-resolve-generate-result' resolved result
  (generate-result.match result
    (fn degenerate' (impure (pair resolved degenerate')))
    (mpm-resolve-generating mpm-resolve-generate-result' resolved)
    (mpm-resolve-generated mpm-resolve-generate-result' resolved)))

;; Resolves a generating with mpm.
(defn mpm-resolve-generating resolve resolved generating'
  (then-run-with
    (mpm-resolve-dependencies resolve resolved generating' (generating.dependencies generating') false)
    (fn pair (resolve (first pair) (second pair)))))

;; Resolves a generated with mpm.
(defn mpm-resolve-generated resolve resolved generated'
  (let unresolved (global-env.unresolved (generated.global-env generated'))
    (if (nil? unresolved)
      (impure (pair resolved generated'))
      (then-run-with
        (mpm-resolve-dependencies resolve resolved generated' unresolved false)
        (fn pair (resolve (first pair) (second pair)))))))

;; Resolves a list of dependencies with mpm.
(defn mpm-resolve-dependencies resolve resolved result dependencies indef
  (if (nil? dependencies)
    (let result
      (if indef result
        (degenerate
          (symbol.+ (symbol "Could not find ")
            (flat-map dependencies ((swap append) space)))
          (generate-result.global-env result)))
      (impure (pair resolved result)))
    (let ref-file (mpm-get-ref (car dependencies))
      (then-run-with (file.exists? ref-file)
      (fn is-ref
        (if (not is-ref)
          (mpm-resolve-dependencies resolve resolved result (cdr dependencies) indef)
          (then-run-with (file.read ref-file)
          (fn ref
            (if (some? (tree-map.get resolved ref))
              (mpm-resolve-dependencies resolve resolved result (cdr dependencies) true)
              (then-run-with (parse-file (mpm-get-src ref) () true)
              (fn exprs
                (then-run-with (resolve (tree-map.put resolved ref true) (generate-exprs' exprs result))
                (fn pair
                  (mpm-resolve-dependencies resolve
                    (first pair)
                    (second pair)
                    (cdr dependencies)
                    true))))))))))))))