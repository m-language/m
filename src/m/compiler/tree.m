(def tree/val
  (new-data (symbol tree/val)
    (list (symbol name))))

(def tree/val.name (field (symbol tree/val) (symbol name)))

(def tree/def
  (new-data (symbol tree/def)
    (list (symbol name) (symbol value))))

(def tree/def.name (field (symbol tree/def) (symbol name)))
(def tree/def.value (field (symbol tree/def) (symbol value)))

(def tree/fn
  (new-data (symbol tree/fn)
    (list (symbol arg) (symbol value))))

(def tree/fn.arg (field (symbol tree/fn) (symbol arg)))
(def tree/fn.value (field (symbol tree/fn) (symbol value)))

(def tree/ap
  (new-data (symbol tree/ap)
    (list (symbol fn) (symbol arg))))

(def tree/ap.fn (field (symbol tree/ap) (symbol fn)))
(def tree/ap.arg (field (symbol tree/ap) (symbol arg)))

(def tree/symbol
  (new-data (symbol tree/symbol)
    (list (symbol name))))

(def tree/symbol.name (field (symbol tree/symbol) (symbol name)))

(def tree/nil (tree/symbol (symbol "nil")))
