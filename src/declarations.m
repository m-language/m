;; A def declaration.
(def def-declaration
  (new-data (symbol def-declaration)
    (list3 (symbol name) (symbol path) (symbol value))))

(def def-declaration.name
  (field (symbol def-declaration) (symbol name)))

(def def-declaration.path
  (field (symbol def-declaration) (symbol path)))

(def def-declaration.value
  (field (symbol def-declaration) (symbol value)))

(def def-declaration? (is? (symbol def-declaration)))

;; A function declaration.
(def fn-declaration
  (new-data (symbol fn-declaration)
    (list4 (symbol name) (symbol path) (symbol closures) (symbol value))))

(def fn-declaration.name
  (field (symbol fn-declaration) (symbol name)))

(def fn-declaration.path
  (field (symbol fn-declaration) (symbol path)))

(def fn-declaration.closures
  (field (symbol fn-declaration) (symbol closures)))

(def fn-declaration.value
  (field (symbol fn-declaration) (symbol value)))

(def fn-declaration? (is? (symbol fn-declaration)))

;; The name of a declaration.
(def declaration.name
  (fn declaration
    (if (def-declaration? declaration)
      (def-declaration.name declaration)
      (fn-declaration.name declaration))))

;; The value of a declaration.
(def declaration.value
  (fn declaration
    (if (def-declaration? declaration)
      (def-declaration.value declaration)
      (fn-declaration.value declaration))))

;; Converts a list of declarations to a map.
(def declarations->declaration-map
  (fn declarations
    (fold declarations (empty-tree-map compare-symbol)
      (fn map
        (fn declaration
          (tree-map.put map (declaration.name declaration) declaration))))))

;; Sorts a list of declarations by their dependencies.
(def sort-declarations
  (fn declarations
    (sort-declaration-map
      (declarations->declaration-map declarations))))

;; Sorts a map of declarations by their dependencies.
(def sort-declaration-map
  (fn map
    (reverse
      (second
        (tree-map.fold map
          (pair (empty-tree-map compare-symbol) ())
          (declaration-dependencies map))))))

;; A list of dependencies of a declaration.
(def declaration-dependencies
  (fn map
    (fn acc
      (fn name
        (fn declaration
          (if (some? (tree-map.get (first acc) name))
            acc
            (with
              (operation.fold (declaration.value declaration)
                (pair
                  (tree-map.put (first acc) name true)
                  (second acc))
                (fn acc
                  (fn operation
                    (if (is? (symbol global-variable-operation) operation)
                      (declaration-dependencies map acc
                        (global-variable-operation.name operation)
                        (unnull (tree-map.get map (global-variable-operation.name operation))))
                      acc))))
            (fn new-acc
              (pair
                (first new-acc)
                (cons declaration (second new-acc)))))))))))