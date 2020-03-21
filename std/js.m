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

  # a.bind(b)
  (def bind (. module "bind"))

  # o.method.bind(o) 
  (defn (invoke object method) (bind (. object method) object))

  # a.apply(b, c)
  (def apply (. module "apply"))

  # a.apply(b, [])
  (defn (force f this) (apply f this (array empty)))

   # Create an instance of an object from a constructor
  (defn (new constructor) (bind constructor (object create (prototype constructor))))

  # Make sure n arguments are supplied before invoking the function
  (defn (partial n value)
    (int gt n 1
      (fn arg (partial (int sub n 1) (bind value null arg)))
      (fn arg (apply value null (array of arg)))))

  (defmodule console {
    (def log (. (. module "console") "log"))
  })

  (defmodule json {
    (def stringify (. (. module "JSON") "stringify"))
  })

  (defmodule boolean {
    (def constructor (new (. module "Boolean")))
    (def true (constructor 1))
    (def false (constructor 0))
  })

  (defmodule object {
    (defn (create prototype) (. (. module "Object") "create" prototype))
    (def empty (from-entries (array empty)))
    
    (defn (from-entries entries)
      (. (. module "Object") 
         "fromEntries" entries))
    
    (defn (entries o) (. (. module "Object") "entries" o))
    
    (defn (of key value)
      (from-entries (array of (array concat (array of key) (array of value)))))
    
    (defn (concat a b)
      (from-entries (array concat (entries a) (entries b))))

    (defn (copy a) (. (. module "Object" ) "assign" a))
    
    (defn (set o key value)
      (with (copy o)
        (fn obj (. (. module "Object") "defineProperty" obj key (of "value" value)))))
  })

  (defmodule array {
    (def constructor (new (. module "Array")))
    (def empty ((. module "Array") 0))
    (defn (of value) ((. (. module "Array") "of") value))
    (defn (concat a b) (invoke a "concat" b))
    (defn (copy arr) (force (invoke arr "slice") null))
    (defn (push array value) (with (copy array) (fn arr (invoke arr "push" value))))
  })
})