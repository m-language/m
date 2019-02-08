;;; Data.m
;;;
;;; An implementation of generic data structures whose type is encoded as a
;;; symbol, and whose contents are encoded as a function from a symbol to the
;;; value of the field with that symbol.
;;;
;;; All definitions in this file are optimized using the backend's native
;;; implementation of generic data structures.

;; The type of a data structure.
(def type-name first)

;; Tests if a data structure is of a type.
(def is?
  (fn type
    (fn data
      (ap symbol.= type (ap type-name data)))))

;; Casts a data structure to a type.
(def as
  (fn type
    (fn data
      (if (ap is? type data)
        data
        (ap error (ap concat (ap symbol->list (symbol "Could not cast "))
                  (ap concat (ap symbol->list (ap type-name data))
                  (ap concat (ap symbol->list (symbol " to "))
                    (ap symbol->list type)))))))))

;; Derives a data structure with a field.
(def derive
  (fn data
    (fn name
      (fn value
        (ap pair (ap first data)
          (fn field
            (if (ap symbol.= field name)
              value
              (ap (ap second data) field))))))))

;; Creates an empty data structure given a type.
(def object
  (fn type
    (ap pair type
      (fn name
        (ap concat (ap symbol->list (symbol "Could not find field "))
        (ap concat (ap symbol->list name)
        (ap concat (ap symbol->list (symbol " for "))
          (ap symbol->list type))))))))

;; Creates a data structure given a type and a list of fields.
(def data
  (fn type
    (fn fields
      (ap fold fields (ap object type)
        (fn data
          (fn field
            (ap derive data (ap first field) (ap second field))))))))

;; Creates a data structure'constructor given a type and a list of field names.
(def new-data
  (fn type
    (fn names
      (ap new-data' type names ()))))

;; Implementation of new-data.
(def new-data'
  (fn type
    (fn names
      (fn fields
        (if (ap nil? names)
          (ap data type fields)
          (fn value
            (ap new-data'
              type
              (ap cdr names)
              (ap cons (ap pair (ap car names) value) fields))))))))

;; Gets the value of a field in a data structure.
(def field
  (fn type
    (fn name
      (fn data
        (ap (ap second (ap as type data)) name)))))