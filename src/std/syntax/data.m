;; Defines a data type given a name and a list of constructors.
(macrofn defdata env exprs
  (if (nil? exprs) (result/error (symbol "Could not match [name type] => (defdata name type*)"))
    (expr/match (car exprs)
      (fn name
        ((data/parse-types name (cdr exprs))
          (fn message (result/error message))
          (fn types
            (result/success
              (quote
                (block
                  (unquote (data/meta name types))
                  (splice (map types data/type-meta))
                  (splice (data/constructors name types))
                  (unquote (data/match name types))))))))
      (fn list (result/error (symbol "Data name must be a symbol"))))))

(defnrec data/parse-types name exprs
  (if (nil? exprs) (right nil)
    (expr/match (car exprs)
      (fn type-name
        ((data/parse-types name (cdr exprs)) left
          (fn types
            (right (cons (pair (concat name (cons slash type-name)) nil) types)))))
      (fn params
        (expr/match (car params)
          (fn type-name
            ((data/parse-fields (cdr params)) left
              (fn fields
                ((data/parse-types name (cdr exprs)) left
                  (fn types
                    (right (cons (pair (concat name (cons slash type-name)) fields) types)))))))
          (fn list (left (symbol "Type name must be a symbol"))))))))

(defnrec data/parse-fields exprs
  (if (nil? exprs) (right nil)
    (expr/match (car exprs)
      (fn name
        ((data/parse-fields (cdr exprs)) left
          (fn fields
            (right (cons name fields)))))
      (fn list (left (symbol "Field name must be a symbol"))))))

(defn data/meta name types
  (let types-list
         (map types
           (fn type
             (quote (symbol (unquote (expr/symbol (first type)))))))
    (quote
      (def (unquote (expr/symbol (concat name (symbol "-constructors"))))
        (list (splice types-list))))))

(defn data/type-meta type
  (let name (first type)
       fields (second type)
       fields-exprs
         (map fields
           (fn field
             (quote (symbol (unquote (expr/symbol field))))))
    (quote
      (def (unquote (expr/symbol (concat name (symbol "-fields"))))
        (list (splice fields-exprs))))))

(defn data/constructors name types
  (map types (data/constructor name types)))

; (defn type field1 field2
;   (fn type1 type2
;     (type (fn type (type field1 field2)))))
(defn data/constructor name types type
  (let type-name (first type)
       fields (second type)
       field-exprs (map fields expr/symbol)
       param-exprs (map types (fn t (expr/symbol (first t))))
    (quote
      (defn (unquote (expr/symbol type-name)) (splice field-exprs)
        (fn (splice param-exprs)
          ((unquote (expr/symbol type-name))
            (fn (unquote (expr/symbol type-name))
              ((unquote (expr/symbol type-name)) (splice field-exprs)))))))))

; (defn name.match name
;   (fn type1 type2
;     (name (with type1) (with type2)))
(defn data/match name types
  (let match-name (concat name (symbol ".match"))
       type-names (map types first)
       type-case-exprs
         (map type-names
           (fn type-name
             (quote (with (unquote (expr/symbol type-name))))))
    (quote
      (defn (unquote (expr/symbol match-name)) (unquote (expr/symbol name))
        (fn (splice (map type-names expr/symbol))
          ((unquote (expr/symbol name)) (splice type-case-exprs)))))))
