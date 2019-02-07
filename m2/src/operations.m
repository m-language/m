;; Operation for a local variable.
(def local-variable-operation
  (ap new-data (symbol local-variable-operation)
    (ap list2 (symbol name) (symbol index))))

(def local-variable-operation.name
  (ap field (symbol local-variable-operation) (symbol name)))

(def local-variable-operation.index
  (ap field (symbol local-variable-operation) (symbol index)))

;; Operation for a global variable.
(def global-variable-operation
  (ap new-data (symbol global-variable-operation)
    (ap list2 (symbol name) (symbol path))))

(def global-variable-operation.name
  (ap field (symbol global-variable-operation) (symbol name)))

(def global-variable-operation.path
  (ap field (symbol global-variable-operation) (symbol path)))

;; An if operation.
(def if-operation
  (ap new-data (symbol if-operation)
    (ap list3 (symbol cond) (symbol true) (symbol false))))

(def if-operation.cond
  (ap field (symbol if-operation) (symbol cond)))

(def if-operation.true
  (ap field (symbol if-operation) (symbol true)))

(def if-operation.false
  (ap field (symbol if-operation) (symbol false)))

;; A def operation.
(def def-operation
  (ap new-data (symbol def-operation)
    (ap list3 (symbol name) (symbol path) (symbol value))))

(def def-operation.name
  (ap field (symbol def-operation) (symbol name)))

(def def-operation.path
  (ap field (symbol def-operation) (symbol path)))

(def def-operation.value
  (ap field (symbol def-operation) (symbol value)))

;; A function operation.
(def fn-operation
  (ap new-data (symbol fn-operation)
    (ap list3 (symbol path) (symbol name) (symbol closures))))

(def fn-operation.path
  (ap field (symbol fn-operation) (symbol path)))

(def fn-operation.name
  (ap field (symbol fn-operation) (symbol name)))

(def fn-operation.closures
  (ap field (symbol fn-operation) (symbol closures)))

;; A do operation
(def do-operation
  (ap new-data (symbol do-operation)
    (ap list1 (symbol operation))))

(def do-operation.operation
  (ap field (symbol do-operation) (symbol operation)))

;; A symbol operation.
(def symbol-operation
  (ap new-data (symbol symbol-operation)
    (ap list1 (symbol name))))

(def symbol-operation.name
  (ap field (symbol symbol-operation) (symbol name)))

;; An apply operation.
(def apply-operation
  (ap new-data (symbol apply-operation)
    (ap list2 (symbol fn) (symbol arg))))

(def apply-operation.fn
  (ap field (symbol apply-operation) (symbol fn)))

(def apply-operation.arg
  (ap field (symbol apply-operation) (symbol arg)))

;; Combines two operations.
(def combine-operation
  (ap new-data (symbol combine-operation)
    (ap list2 (symbol first) (symbol second))))

(def combine-operation.first
  (ap field (symbol combine-operation) (symbol first)))

(def combine-operation.second
  (ap field (symbol combine-operation) (symbol second)))

;; Marks an operation with a line number.
(def line-number-operation
  (ap new-data (symbol line-number-operation)
    (ap list2 (symbol operation) (symbol line))))

(def line-number-operation.operation
  (ap field (symbol line-number-operation) (symbol operation)))

(def line-number-operation.line
  (ap field (symbol line-number-operation) (symbol line)))

;; The nil operation.
(def nil-operation (ap object (ap symbol nil-operation)))