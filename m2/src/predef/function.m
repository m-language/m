;; Composes two functions [f] and [g].
(def compose
  (lambda f
    (lambda g
      (lambda x
        (f (g x))))))