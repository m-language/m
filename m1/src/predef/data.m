;; The type of a data object.
(def type-name left)

;; Tests if [data] is of [type].
(def is?
  (lambda type
    (lambda data
      (symbol.= type (type-name data)))))

;; Casts [data] to [type].
(def as
  (lambda type
    (lambda data
      (if (is? type data)
        data
        (error (symbol.+ (symbol "Could not cast ")
               (symbol.+ (type-name data)
               (symbol.+ (symbol " to ")
                 type))))))))

;; Derives a data object with a field.
(def derive
  (lambda data
    (lambda name
      (lambda value
        (pair (left data)
          (lambda field
            (if (symbol.= field name)
              value
              ((right data) field))))))))

;; Creates an empty data object with type [type].
(def object
  (lambda type
    (pair type
      (lambda name
        (symbol.+ (symbol "Could not find field ")
        (symbol.+ name
        (symbol.+ (symbol " for ")
          type)))))))

;; Creates a data object given a [type] and a list of [fields].
(def data
  (lambda type
    (lambda fields
      (fold fields (object type)
        (lambda data
          (lambda field
            (derive data (left field) (right field))))))))

;; Creates a data constructor given a [type] and a list of field [names].
(def new-data
  (lambda type
    (lambda names
      (new-data' type names ()))))

;; Implementation of [new-data].
(def new-data'
  (lambda type
    (lambda names
      (lambda fields
        (if (nil? names)
          (data type fields)
          (lambda value
            (new-data'
              type
              (cdr names)
              (cons (pair (car names) value) fields))))))))

;; Variadic implemntation of [new-data].
(def new-data*
  (lambda type
    (vararg (new-data type))))

;; Gets a field in a data object.
(def field
  (lambda type
    (lambda name
      (lambda data
        ((right (as type data)) name)))))