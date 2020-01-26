#(An implementation of booleans as two argument functions which ignore one 
  argument, where (true x y) = (x) and (false x y) = (y))
(defmodule bool {
  #(The singleton truthy value, a function which ignores its second argument)
  (defn (true x y) x)

  #(The singleton falsy value, a function which ignores its first argument)
  (defn (false x y) y)

  #(True if its argument is false)
  (defn (not x) 
    (x false true))

  #(True if both arguments are true)
  (defn (and x y) 
    (x y false))

  #(True if either argument is true)
  (defn (or x y) 
    (x true y))
})
