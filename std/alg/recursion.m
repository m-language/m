(defn (fix f)
  ((fn x (f (fn v (x x v))))
   (fn x (f (fn v (x x v))))))