;;; Data.m
;;;
;;; An implementation of generic data structures whose type is encoded as a
;;; symbol and whose contents are encoded as a function from a symbol to the
;;; value of the field with that symbol.
;;;
;;; All definitions in this file are optimized using the backend's native
;;; implementation of generic data structures.

;; The type of a data structure.
(def type-name first)

;; Tests if a data structure is of a type.
(defn is? type data
  (symbol.= type (type-name data)))

;; Casts a data structure to a type.
(defn as type data
  (if (is? type data) data
    (error (concat (symbol->list (symbol "Could not cast "))
           (concat (symbol->list (type-name data))
           (concat (symbol->list (symbol " to "))
             (symbol->list type)))))))

;; Derives a data structure with a field.
(defn derive data name value
  (pair (first data)
    (fn field
      (if (symbol.= field name) value
        (second data field)))))

;; Creates an empty data structure given a type.
(defn object type
  (pair type
    (fn name
      (concat (symbol->list (symbol "Could not find field "))
      (concat (symbol->list name)
      (concat (symbol->list (symbol " for "))
        (symbol->list type)))))))

;; Creates a data structure given a type and a list of fields.
(defn data type fields
  (fold fields (object type)
    (fn data field
      (derive data (first field) (second field)))))

;; Creates a data structure's constructor given a type and a list of field names.
(defn new-data type names
  (new-data' type names ()))

;; Implementation of new-data.
(defnrec new-data' type names fields
  (if (nil? names)
    (data type fields)
    (fn value
      (new-data' type
        (cdr names)
        (cons (pair (car names) value) fields)))))

;; Gets the value of a field in a data structure.
(defn field type name data
  ((second (as type data)) name))
