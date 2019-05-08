;; Creates a successful result.
(def result/success
  (fn expr
    (left expr)))

;; Creates an error result.
(defn result/error error
  (right (left error)))

;; Creates a result which depends on a value.
(defn result/depends dependencies
  (right (right dependencies)))

;; Matches the value of a result.
(defn result/match result success error depends
  (result success
    (fn fail
      (fail error depends))))
