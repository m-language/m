#(Module for using M strings)
(defpackage string [length get get-unsafe] {
  #(Similar to get, but errors if the index is out of bounds)
  (defn (get-unsafe string index) 
    (get string index (error "Index out of bounds")))
})
