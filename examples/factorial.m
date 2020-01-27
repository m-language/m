#(A generic factorial function parameterized over a numeric module)
(defn (factorial num x)
  (import num
    ((eq x zero) one
      (mul x (factorial num (sub x one))))))

#(The factorial function specialized for integers)
(def factorial-int (factorial int))
