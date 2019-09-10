;; A def declaration.
(def def-declaration
  (new-data (symbol def-declaration)
    (list (symbol name) (symbol path) (symbol value))))

(def def-declaration.name (field (symbol def-declaration) (symbol name)))
(def def-declaration.path (field (symbol def-declaration) (symbol path)))
(def def-declaration.value (field (symbol def-declaration) (symbol value)))
(def def-declaration? (is? (symbol def-declaration)))

;; A function declaration.
(def fn-declaration
  (new-data (symbol fn-declaration)
    (list (symbol name) (symbol path) (symbol closures) (symbol value))))

(def fn-declaration.name (field (symbol fn-declaration) (symbol name)))
(def fn-declaration.path (field (symbol fn-declaration) (symbol path)))
(def fn-declaration.closures (field (symbol fn-declaration) (symbol closures)))
(def fn-declaration.value (field (symbol fn-declaration) (symbol value)))
(def fn-declaration? (is? (symbol fn-declaration)))

;; The name of a declaration.
(defn declaration.name declaration
  (if (def-declaration? declaration)
    (def-declaration.name declaration)
    (fn-declaration.name declaration)))

;; The value of a declaration.
(defn declaration.value declaration
  (if (def-declaration? declaration)
    (def-declaration.value declaration)
    (fn-declaration.value declaration)))

(defnrec declarations->trees declarations
  (if (nil? declarations) nil
    (let declaration (car declarations)
         type (type-name declaration)
      (pcond (symbol.= type)
        (symbol def-declaration)
          (cons
            (tree/def (def-declaration.name declaration) (operation->tree (def-declaration.value declaration)))
            (declarations->trees (cdr declarations)))
        (symbol fn-declaration)
          (declarations->trees (cdr declarations))
        (error (symbol "..."))))))
