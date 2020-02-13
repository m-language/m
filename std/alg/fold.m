#(Module for folds)
(defmodule fold {
  #(Folds a string)
  (defn (from-string str acc f)
    (string case str acc
      (fn [car cdr]
        (from-string cdr (f acc car) f))))
})
