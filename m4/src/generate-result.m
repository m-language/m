(include data)
(include symbol)

;; The result of generating an expr.
(def generate-result
  (lambda operation
    (lambda declaration
      (lambda env
        (derive (symbol generate-result) (symbol operation) operation
        (derive (symbol generate-result) (symbol declaration) declaration
        (derive (symbol generate-result) (symbol env) env
          (object (symbol generate-result)))))))))

(def generate-result.operation
  (field (symbol generate-result) (symbol operation)))

(def generate-result.declaration
  (field (symbol generate-result) (symbol declaration)))

(def generate-result.env
  (field (symbol generate-result) (symbol env)))