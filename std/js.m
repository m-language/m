(defmodule js {
  (def index (extern "index"))

  (defn (. object prop) (index object prop))
  
  (def module (extern "js"))

  (def true (. module "true"))
  (def false (. module "false"))
  (def null (. module "null"))
  (def undefined (. module "undefined"))
  (def empty (. module "empty"))
  (def apply (. module "apply"))
  (def call (. module "call"))
  (defn (bind object method) (call (. object method) object))
  
  (def log (bind (. module "console") "log"))

  (defn (new constructor) (call constructor empty))

  (defmodule array {
    (def array-constructor (new (. module "Array")))
    (def array-prototype (. (. module "Array") "prototype"))
    (def empty (array-constructor 0))
    (defn (push array value) (apply (bind array "slice" undefined) (fn arr (bind arr "push" value))))
    (defn (of value) (push empty value))
  })
})