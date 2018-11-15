;; Operation which pushes a local variable.
(def local-variable-operation
  (lambda name
    (lambda index
      (derive (symbol local-variable-operation) (symbol name) name
      (derive (symbol local-variable-operation) (symbol index) index
        (object (symbol local-variable-operation)))))))

;; Operation which pushes a global variable.
(def global-variable-operation
  (lambda name
    (lambda path
      (derive (symbol global-variable-operation) (symbol name) name
      (derive (symbol global-variable-operation) (symbol path) path
        (object (symbol global-variable-operation)))))))

;; Operation which pushes a reflective variable.
(def reflective-variable-operation
  (lambda name
    (lambda path
      (derive (symbol reflective-variable-operation) (symbol name) name
      (derive (symbol reflective-variable-operation) (symbol path) path
        (object (symbol reflective-variable-operation)))))))

;; An if operation.
(def if-operation
  (lambda cond
    (lambda true
      (lambda false
        (derive (symbol if-operation) (symbol cond) cond
        (derive (symbol if-operation) (symbol true) true
        (derive (symbol if-operation) (symbol false) false
          (object (symbol if-operation)))))))))

;; A def operation.
(def def-operation
  (lambda name
    (lambda value
      (lambda path
        (derive (symbol def-operation) (symbol name) name
        (derive (symbol def-operation) (symbol value) value
        (derive (symbol def-operation) (symbol path) path
          (object (symbol def-operation)))))))))

;; A lambda operation.
(def lambda-operation
  (lambda path
    (lambda name
      (lambda closures
        (derive (symbol lambda-operation) (symbol path) path
        (derive (symbol lambda-operation) (symbol name) name
        (derive (symbol lambda-operation) (symbol closures) closures
          (object (symbol lambda-operation)))))))))

;; A symbol operation.
(def symbol-operation
  (lambda name
    (derive (symbol symbol-operation) (symbol name) name
      (object (symbol symbol-operation)))))

;; An import operation.
(def import-operation
  (lambda name
    (derive (symbol symbol-operation) (symbol name) name
      (object (symbol symbol-operation)))))

;; An apply operation.
(def apply-operation
  (lambda fn
    (lambda arg
      (derive (symbol apply-operation) (symbol fn) fn
      (derive (symbol apply-operation) (symbol arg) arg
        (object (symbol apply-operation)))))))

;; Combines two operations.
(def combine-operation
  (lambda first
    (lambda second
      (derive (symbol combine-operation) (symbol first) first
      (derive (symbol combine-operatoin) (symbol second) second
        (object (symbol combine-operation)))))))

;; Marks an operation with a line number.
(def line-number-operation
  (lambda operation
    (lambda line
      (derive (symbol line-number-operation) (symbol operation) operation
      (derive (symbol line-number-operation) (symbol line) line
        (object (symbol line-number-operation)))))))

;; The nil operation.
(def nil-operation (object (symbol nil-operation)))