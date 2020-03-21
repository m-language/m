(defmodule js {
  # Access a property of an object
  (def index (extern "index"))
  (defn (. object prop) (index object prop))
  
  (def module (extern "js"))

  (defn (prototype of) (. of "prototype"))

  # Simple primitive values
  (def null (. module "null"))
  (def undefined (. module "undefined"))

  # Apply a function to a value, then return the value
  (def with (. module "with"))

  # a.bind(b, c)
  (def bind (. module "bind"))

  # a.apply(b, c)
  (def apply (. module "apply"))

  # a.apply(b, [])
  (defn (force f this) (apply f this (array empty)))

  # Make sure n arguments are supplied before invoking the function
  (defn (partial n value)
    (int gt n 1
      (fn arg (partial (int sub n 1) (bind value null arg)))
      (fn arg (apply value null (array of arg)))))

  # o.bind(o) 
  (defn (invoke object method) (bind (. object method) object))

   # Create an instance of an object from a constructor
  (defn (new constructor) (bind constructor (object create (. constructor "prototype"))))

  (defmodule object {
    (defn (create prototype) (. (. module "Object") "create" prototype))
    (def empty (create (. module "Object")))
  })
  
  (defmodule console {
    (def log (. (. module "console") "log"))
  })

  (defmodule boolean {
    (def constructor (new (. module "Boolean")))
    (def true (constructor 1))
    (def false (constructor 0))
  })

  (defmodule array {
    (def constructor (new (. module "Array")))
    
    (def empty ((. module "Array") 0))

    (defn (copy array) (invoke array "slice" undefined))
    (defn (push array value) (with (copy array) (fn arr (bind arr "push" value))))
    (defn (of value) (push empty value))
    (defn (append a b) (invoke a "concat" b))
  })
})