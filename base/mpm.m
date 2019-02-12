;; The root file of the local mpm storage.
(def mpm-root mpm-root)

;; The root file of the local mpm reference storage.
(def mpm-ref-root (file.child mpm-root (symbol ref)))

;; The root file of the local mpm source storage.
(def mpm-src-root (file.child mpm-root (symbol src)))

;; Gets the reference in mpm-root given a name.
(def mpm-get-ref
  (fn name
    (file.read (file.child mpm-ref-root (normalize name)))))

;; Gets the source in mpm-root given a reference.
(def mpm-get-src
  (fn ref
    (file.read (file.child mpm-src-root (concat ref (symbol ".m"))))))

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

;; Replaces all special symbols.
(def normalize id)

;; Retrieves all special symbols.
(def unnormalize id)