;; Operation for a local variable.
(def local-variable-operation
  (new-data (symbol local-variable-operation)
    (list2 (symbol name) (symbol index))))

;; Operation for a global variable.
(def global-variable-operation
  (new-data (symbol global-variable-operation)
    (list2 (symbol name) (symbol path))))

;; An if operation.
(def if-operation
  (new-data (symbol if-operation)
    (list3 (symbol cond) (symbol true) (symbol false))))

;; A def operation.
(def def-operation
  (new-data (symbol def-operation)
    (list3 (symbol name) (symbol path) (symbol value))))

;; A lambda operation.
(def lambda-operation
  (new-data (symbol lambda-operation)
    (list3 (symbol path) (symbol name) (symbol closures))))

;; A symbol operation.
(def symbol-operation
  (new-data (symbol symbol-operation)
    (list1 (symbol name))))

;; An apply operation.
(def apply-operation
  (new-data (symbol apply-operation)
    (list2 (symbol fn) (symbol arg))))

;; Combines two operations.
(def combine-operation
  (new-data (symbol combine-operation)
    (list2 (symbol first) (symbol second))))

;; Marks an operation with a line number.
(def line-number-operation
  (new-data (symbol line-number-operation)
    (list2 (symbol operation) (symbol line))))

;; The nil operation.
(def nil-operation (object (symbol nil-operation)))