;; The root file of the local mpm storage.
(extern mpm-root)

;; The root file of the local mpm reference storage.
(def mpm-ref-root (ref-root mpm-root))

(defn ref-root root (file.child root (symbol ref)))

;; The root file of the local mpm source storage.
(def mpm-src-root (src-root mpm-root))

(defn src-root root (file.child root (symbol src)))

;; Replaces all special symbols.
(def normalize id)

;; Retrieves all special symbols.
(def unnormalize id)

;; Gets the reference file in mpm-root given a name.
(def mpm-get-ref (get-ref mpm-ref-root))

(defn get-ref ref-root name
  (file.child ref-root (normalize name)))

;; Gets the source file in mpm-root given a reference.
(def mpm-get-src (get-src mpm-src-root))

(defn get-src src-root ref
  (file.child src-root (concat ref (symbol ".m"))))

;; Puts all declarations in mpm-root as references.
(defn mpm-put-refs declarations (put-refs declarations mpm-ref-root))

(defn put-refs declarations ref-root
  (let def-declarations (filter declarations def-declaration?)
    (fold def-declarations (impure ())
      (fn process declaration
        (then-run process
          (file.write
            (file.child ref-root
              (normalize (def-declaration.name declaration)))
            (def-declaration.path declaration)))))))

;; Puts all sources in mpm-root.
(defn mpm-put-srcs in (put-srcs in mpm-src-root))

(defn put-srcs in src-root
  (file.copy in src-root))

;; Resolves a generate result with mpm.
(defn mpm-resolve-generate-result result
  ((swap run-with) second
    (mpm-resolve-generate-result' (empty-tree-map compare-symbol) result)))

(defnrec mpm-resolve-generate-result' resolved result
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
(def mpm-resolve-dependencies (resolve-dependencies mpm-root))

(defnrec resolve-dependencies root resolve resolved result dependencies resolved?
  (let src-of-ref (get-src (src-root root)) 
       ref-of-sym (get-ref (ref-root root))
    (if (nil? dependencies)
      (let result
        (if resolved? result
          (degenerate
            (list (symbol "Could not resolve dependencies"))
            (generate-result.global-env result)))
        (impure (pair resolved result)))
      (let ref-file (ref-of-sym (car dependencies))
        (do is-ref (file.exists? ref-file)
          (if (not is-ref)
            (resolve-dependencies root resolve resolved result (cdr dependencies) resolved?)
            (do ref (file.read ref-file)
              (if (some? (tree-map.get resolved ref))
                (resolve-dependencies root resolve resolved result (cdr dependencies) true)
                (do exprs (parse-file (src-of-ref ref))
                    pair (resolve (tree-map.put resolved ref true) (generate-exprs' exprs result))
                  (resolve-dependencies
                    root
                    resolve
                    (first pair)
                    (second pair)
                    (cdr dependencies)
                    true))))))))))
