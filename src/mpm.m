;; The root file of the local mpm storage.
(def mpm-root mpm-root)

;; The root file of the local mpm reference storage.
(def mpm-ref-root (file.child mpm-root (symbol ref)))

;; The root file of the local mpm source storage.
(def mpm-src-root (file.child mpm-root (symbol src)))

;; Replaces all special symbols.
(def normalize id)

;; Retrieves all special symbols
(def unnormalize id)

;; Gets the reference file in mpm-root given a name.
(def mpm-get-ref
  (fn name
    (file.child mpm-ref-root (normalize name))))

;; Gets the source file in mpm-root given a reference.
(def mpm-get-src
  (fn ref
    (file.child mpm-src-root (concat ref (symbol ".m")))))

;; Puts all declarations in mpm-root as references.
(def mpm-put-refs
  (fn declarations
    (with (filter declarations def-declaration?)
    (fn def-declarations
      (fold def-declarations (impure ())
        (fn process
          (fn declaration
            (then-run process
              (file.write
                (file.child mpm-ref-root
                  (normalize (def-declaration.name declaration)))
                (def-declaration.path declaration))))))))))

;; Puts all sources in mpm-root.
(def mpm-put-srcs
  (fn in
    (file.copy in mpm-src-root)))

;; Puts a file in mpm.
(def mpm-put
  (fn in
    (then-run-with (generate in)
    (fn result
      (then-run
        (mpm-put-refs (generated.declarations result))
        (mpm-put-srcs in))))))

;; Resolves a generate result with mpm.
(def mpm-resolve-generate-result
  (fn result
    ((swap run-with) second
      (mpm-resolve-generate-result' (empty-tree-map compare-symbol) result))))

(def mpm-resolve-generate-result'
  (fn resolved result
    (generate-result.match result
      (fn degenerate' (impure (pair resolved degenerate')))
      (mpm-resolve-generating mpm-resolve-generate-result' resolved)
      (mpm-resolve-generated mpm-resolve-generate-result' resolved))))

;; Resolves a generating with mpm.
(def mpm-resolve-generating
  (fn resolve resolved generating'
    (then-run-with
      (mpm-resolve-dependencies resolve resolved generating' (generating.dependencies generating'))
      (fn pair (resolve (first pair) (second pair))))))

;; Resolves a generated with mpm.
(def mpm-resolve-generated
  (fn resolve resolved generated'
    (with (global-env.unresolved (generated.global-env generated'))
    (fn unresolved
      (if (nil? unresolved)
        (impure (pair resolved generated'))
        (then-run-with
          (mpm-resolve-dependencies resolve resolved generated' unresolved)
          (fn pair (resolve (first pair) (second pair)))))))))

;; Resolves a list of dependencies with mpm.
(def mpm-resolve-dependencies
  (fn resolve resolved result dependencies
    (if (nil? dependencies)
      (impure (pair resolved result))
      (with (mpm-get-ref (car dependencies))
      (fn ref-file
        (then-run-with (file.exists? ref-file)
        (fn is-ref
          (if (not is-ref)
            (mpm-resolve-dependencies resolve resolved result (cdr dependencies))
            (then-run-with (file.read ref-file)
            (fn ref
              (if (some? (tree-map.get resolved ref))
                (mpm-resolve-dependencies resolve resolved result (cdr dependencies))
                (then-run-with (parse-file (mpm-get-src ref) () true)
                (fn exprs
                  (then-run-with (resolve (tree-map.put resolved ref true) (generate-exprs' exprs result))
                  (fn pair
                    (mpm-resolve-dependencies resolve
                      (first pair)
                      (second pair)
                      (cdr dependencies)))))))))))))))))