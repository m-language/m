#(Module for folds)
(defmodule fold {
  #(Folds a string)
  (defn (string->fold str acc f)
    (string case str acc
      [car cdr]
        (string->fold cdr (f acc car) f)))
})
