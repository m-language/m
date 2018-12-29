;; Operation for a local variable.
(def local-variable-operation
  (new-data* (symbol local-variable-operation)
    (symbol name) (symbol index) ()))

;; Operation for a global variable.
(def global-variable-operation
  (new-data* (symbol global-variable-operation)
    (symbol name) (symbol path) ()))

;; An if operation.
(def if-operation
  (new-data* (symbol if-operation)
    (symbol cond) (symbol true) (symbol false) ()))

;; A def operation.
(def def-operation
  (new-data* (symbol def-operation)
    (symbol name) (symbol path) (symbol value) ()))

;; A lambda operation.
(def lambda-operation
  (new-data* (symbol lambda-operation)
    (symbol path) (symbol name) (symbol closures) ()))

;; A symbol operation.
(def symbol-operation
  (new-data* (symbol symbol-operation)
    (symbol name) ()))

;; An apply operation.
(def apply-operation
  (new-data* (symbol apply-operation)
    (symbol fn) (symbol arg) ()))

;; Combines two operations.
(def combine-operation
  (new-data* (symbol combine-operation)
    (symbol first) (symbol second) ()))

;; Marks an operation with a line number.
(def line-number-operation
  (new-data* (symbol line-number-operation)
    (symbol operation) (symbol line) ()))

;; The nil operation.
(def nil-operation (object (symbol nil-operation)))