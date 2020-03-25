(defmodule js {
  # Access a property of an object
  (def index (extern "index"))
  (defn (. object prop) (index object prop))
  
  (def module (extern "js"))

  (defn (prototype of) (. of "prototype"))

  # Simple primitive values
  (def null (. module "null"))
  (def undefined (. module "undefined"))

  (def with (. module "with"))

  # a.apply(b, c)
  (def apply (. module "apply"))

  # a.apply(b, [])
  (defn (force f this) (apply f this (array empty)))

  # a.bind(b)
  (defn (bind a b) (apply (. a "bind") a (array of b)))

  # o.method.bind(o) 
  (defn (invoke object method) (bind (. object method) object))

  # Create an instance of an object from a constructor
  (defn (new constructor) (bind constructor (object create (prototype constructor))))

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
      (. (. module "Object") "fromEntries" entries))
    
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
    (defn (append array value) (with (copy array) (fn arr (invoke arr "push" value))))
    (defn (from-n-elements n) (foldn n (fn [acc x] (concat acc (of x))) empty))
  })
})
