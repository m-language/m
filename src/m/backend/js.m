(defn js-backend' in out trees
  (file.write out (flat-map trees js/desugar-tree)))

(defnrec js/desugar-tree tree
  (pcond (symbol.= (type-name tree))
    (symbol tree/val)
      (js/desugar-quote (tree/val.name tree))
    (symbol tree/def)
      (let desugared-name (js/desugar-quote (tree/def.name tree))
        (concat (symbol "export const ")
          (concat desugared-name
            (concat (symbol " = ")
              (concat (js/desugar-tree (tree/def.value tree))
                (list semicolon linefeed))))))
    (symbol tree/fn)
      (concat (symbol "RT.createMFunction((")
        (concat (js/desugar-quote (tree/fn.arg tree))
          (concat (symbol ") => ")
            (concat (js/desugar-tree (tree/fn.value tree)) (symbol ")")))))
    (symbol tree/ap)
      (concat (symbol "RT.mCall(")
        (concat (js/desugar-tree (tree/ap.fn tree))
          (concat (symbol ", ")
            (concat (js/desugar-tree (tree/ap.arg tree))
              (symbol ")")))))
    (symbol tree/symbol)
      (concat (symbol "RT.createMSymbol(")
        (concat (js/desugar-string (tree/symbol.name tree))
          (symbol ")")))
    (error (symbol "..."))))

(extern normalize)

;; Quotes a variable with invalid characters.
(defn js/desugar-quote name
  (concat (symbol "m_") (normalize name)))

;; Quotes a string.
(defn js/desugar-string name
  (cons double-quote (append (js/desugar-string' name) double-quote)))

(defnrec js/desugar-string' name
  (cond (nil? name) nil
        (char.= double-quote (car name)) (concat (symbol "\""") (js/desugar-string' (cdr name)))
        (char.= backslash (car name)) (concat (symbol "\\") (js/desugar-string' (cdr name)))
        (char.= linefeed (car name)) (concat (symbol "\n") (js/desugar-string' (cdr name)))
        (char.= carriage-return (car name)) (concat (symbol "\r") (js/desugar-string' (cdr name)))
        (char.= tab (car name)) (concat (symbol "\t") (js/desugar-string' (cdr name)))
    (cons (car name) (js/desugar-string' (cdr name)))))

;;; Externs for js code interop

; Encode an m symbol as a js symbol
(extern js/symbol)

; Get the type of a js value
(extern js/value.type)

;; Get the property of a value
(extern js/value.get-property)

;; Set the property of a value (impure)
(extern js/value.set-property)

;; Call a js method with a `this` and arguments
(extern js/invoke-method)

;; Call a js function
(defn js/call fn args (js/invoke-method fn js/global args))

;; The global `this` value
(extern js/global)

(def js/get-value (js/value.get-property js/global))

(def js/set-value (js/value.set-property js/global))

;; The null value
(extern js/null)

;; The undefined value
(extern js/undefined)

;; Convert an m symbol to a JS string 
(extern js/string)

;; Converts an m bool to a JS bool
(extern js/bool)

;; Convert an m natural to a JS number
(extern js/number)

;; Convert an m association list to a JS object
(extern js/object)

;; Test if two values are monomorphically equal (===)
(extern js/eq?)

; ;; Test if two values polymorphically equal
(extern js/poly-eq?)