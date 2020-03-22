
# Fold over n arguments with an initial value
# Example: Sum 1 through 10
# (foldn 10 (int add) 0 1 2 3 4 5 6 7 8 9 10)

(defn (foldn n concat)
  (import int
    ((eq n 0) id (fn [acc e] (foldn (sub n 1) concat (concat acc e))))))

(defn (length elements)
  ((expr-ops case) elements
    (error "Non-list argument to length")
    0
    (fn [_ rest] ((int add) 1 (length rest)))))

(defm (apply-vararg f args)
  ((expr-ops case) args
    (f args)
    f
    (fn [head tail] ((quote apply-vararg) (f head) tail))))