;; Operation for a local variable.
(def local-variable-operation
  (new-data (symbol local-variable-operation)
    (list2 (symbol name) (symbol index))))

(def local-variable-operation.name
  (field (symbol local-variable-operation) (symbol name)))

(def local-variable-operation.index
  (field (symbol local-variable-operation) (symbol index)))

;; Operation for a global variable.
(def global-variable-operation
  (new-data (symbol global-variable-operation)
    (list2 (symbol name) (symbol path))))

(def global-variable-operation.name
  (field (symbol global-variable-operation) (symbol name)))

(def global-variable-operation.path
  (field (symbol global-variable-operation) (symbol path)))

;; An if operation.
(def if-operation
  (new-data (symbol if-operation)
    (list3 (symbol cond) (symbol true) (symbol false))))

(def if-operation.cond
  (field (symbol if-operation) (symbol cond)))

(def if-operation.true
  (field (symbol if-operation) (symbol true)))

(def if-operation.false
  (field (symbol if-operation) (symbol false)))

;; A def operation.
(def def-operation
  (new-data (symbol def-operation)
    (list3 (symbol name) (symbol path) (symbol value))))

(def def-operation.name
  (field (symbol def-operation) (symbol name)))

(def def-operation.path
  (field (symbol def-operation) (symbol path)))

(def def-operation.value
  (field (symbol def-operation) (symbol value)))

;; A function operation.
(def fn-operation
  (new-data (symbol fn-operation)
    (list3 (symbol path) (symbol name) (symbol closures))))

(def fn-operation.path
  (field (symbol fn-operation) (symbol path)))

(def fn-operation.name
  (field (symbol fn-operation) (symbol name)))

(def fn-operation.closures
  (field (symbol fn-operation) (symbol closures)))

;; A impure operation
(def impure-operation
  (new-data (symbol impure-operation)
    (list1 (symbol operation))))

(def impure-operation.operation
  (field (symbol impure-operation) (symbol operation)))

;; A symbol operation.
(def symbol-operation
  (new-data (symbol symbol-operation)
    (list1 (symbol name))))

(def symbol-operation.name
  (field (symbol symbol-operation) (symbol name)))

;; An apply operation.
(def apply-operation
  (new-data (symbol apply-operation)
    (list2 (symbol fn) (symbol arg))))

(def apply-operation.fn
  (field (symbol apply-operation) (symbol fn)))

(def apply-operation.arg
  (field (symbol apply-operation) (symbol arg)))

;; Combines two operations.
(def combine-operation
  (new-data (symbol combine-operation)
    (list2 (symbol first) (symbol second))))

(def combine-operation.first
  (field (symbol combine-operation) (symbol first)))

(def combine-operation.second
  (field (symbol combine-operation) (symbol second)))

;; Marks an operation with a line number.
(def line-number-operation
  (new-data (symbol line-number-operation)
    (list2 (symbol operation) (symbol line))))

(def line-number-operation.operation
  (field (symbol line-number-operation) (symbol operation)))

(def line-number-operation.line
  (field (symbol line-number-operation) (symbol line)))

;; The nil operation.
(def nil-operation (object (symbol nil-operation)))