#(A generic factorial function parameterized over a numeric module)
(defn-generic [num] (factorial x)
  ((eq x zero) one
    (mul x (factorial (sub x one)))))

#(The factorial function specialized for integers)
(def factorial-int (factorial int))
