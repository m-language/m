(defmodule js {
  (def index (extern "index"))

  (defn (. object prop) (index object prop))
  
  (def module (extern "js"))

  (def true (. module "true"))
  (def false (. module "false"))
  (def null (. module "null"))
  (def undefined (. module "undefined"))

  (defn call (. module "call"))

  (defn (bind object method) (call (. object method) object))
})