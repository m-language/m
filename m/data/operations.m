;; Operation for a local variable.
(def local-variable-operation
  (new-data (symbol local-variable-operation)
    (list (symbol name) (symbol index))))

(def local-variable-operation.name (field (symbol local-variable-operation) (symbol name)))
(def local-variable-operation.index (field (symbol local-variable-operation) (symbol index)))

;; Operation for a global variable.
(def global-variable-operation
  (new-data (symbol global-variable-operation)
    (list (symbol name) (symbol path))))

(def global-variable-operation.name (field (symbol global-variable-operation) (symbol name)))
(def global-variable-operation.path (field (symbol global-variable-operation) (symbol path)))

;; An if operation.
(def if-operation
  (new-data (symbol if-operation)
    (list (symbol cond) (symbol true) (symbol false))))

(def if-operation.cond (field (symbol if-operation) (symbol cond)))
(def if-operation.true (field (symbol if-operation) (symbol true)))
(def if-operation.false (field (symbol if-operation) (symbol false)))

;; A def operation.
(def def-operation
  (new-data (symbol def-operation)
    (list (symbol name) (symbol path) (symbol value))))

(def def-operation.name (field (symbol def-operation) (symbol name)))
(def def-operation.path (field (symbol def-operation) (symbol path)))
(def def-operation.value (field (symbol def-operation) (symbol value)))

;; A function operation.
(def fn-operation
  (new-data (symbol fn-operation)
    (list (symbol path) (symbol name) (symbol arg) (symbol value) (symbol closures))))

(def fn-operation.path (field (symbol fn-operation) (symbol path)))
(def fn-operation.name (field (symbol fn-operation) (symbol name)))
(def fn-operation.arg (field (symbol fn-operation) (symbol arg)))
(def fn-operation.value (field (symbol fn-operation) (symbol value)))
(def fn-operation.closures (field (symbol fn-operation) (symbol closures)))

;; A symbol operation.
(def symbol-operation
  (new-data (symbol symbol-operation)
    (list (symbol name))))

(def symbol-operation.name (field (symbol symbol-operation) (symbol name)))

;; An apply operation.
(def apply-operation
  (new-data (symbol apply-operation)
    (list (symbol fn) (symbol arg))))

(def apply-operation.fn (field (symbol apply-operation) (symbol fn)))
(def apply-operation.arg (field (symbol apply-operation) (symbol arg)))

;; Marks an operation with a line number.
(def line-number-operation
  (new-data (symbol line-number-operation)
    (list (symbol operation) (symbol line))))

(def line-number-operation.operation (field (symbol line-number-operation) (symbol operation)))
(def line-number-operation.line (field (symbol line-number-operation) (symbol line)))

;; The nil operation.
(def nil-operation (object (symbol nil-operation)))

;; Folds over an operation.
(defnrec operation.fold operation acc f
  (f
    (let type (type-name operation)
      (pcond (symbol.= type)
        (symbol local-variable-operation) acc
        (symbol global-variable-operation) acc
        (symbol if-operation)
          (operation.fold (if-operation.false operation)
            (operation.fold (if-operation.true operation)
              (operation.fold (if-operation.cond operation) acc f)
              f)
            f)
        (symbol def-operation) (operation.fold (def-operation.value operation) acc f)
        (symbol fn-operation) (operation.fold (fn-operation.value operation) acc f)
        (symbol symbol-operation) acc
        (symbol apply-operation)
          (operation.fold (apply-operation.arg operation)
            (operation.fold (apply-operation.fn operation) acc f)
            f)
        (symbol line-number-operation) (operation.fold (line-number-operation.operation operation) acc f)
        (symbol nil-operation) acc
        (error (symbol "..."))))
    operation))
