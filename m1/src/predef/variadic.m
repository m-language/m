;; Applies a function which takes a list to a variadic unumber of arguments.
(def vararg
  (lambda f
    (vararg-impl f ())))

(def vararg-impl
  (lambda f
    (lambda acc
      (lambda arg
        (if (nil? arg)
          (f (reverse acc))
          (vararg-impl f (cons arg acc)))))))