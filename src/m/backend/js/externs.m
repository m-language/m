;;; Externs for js code interop

; Fix builds
(macrofn internal env exprs
  (result/success
    (quote
      (def (unquote (car exprs)) id))))

;; Encode an m symbol as a js symbol
(internal js/symbol)

;; Get the type of a js value
(internal js/value.type)

;; Get the property of a value
(internal js/value.get-property)

;; Set the property of a value (impure)
(internal js/value.set-property)

;; Call a js method with a `this` and arguments
(internal js/invoke-method)

;; Call a js function
(defn js/call fn args (js/invoke-method fn js/global args))

;; The global `this` value, whether it be `window`, top-level `this`, or `globalThis`.
(internal js/global)

;; Get a value at the global scope
(def js/get-global-property (js/value.get-property js/global))

;; Set a value at the global scope
(def js/set-global-property (js/value.set-property js/global))

;; The null value
(internal js/null)

;; The undefined value
(internal js/undefined)

;; Convert an m symbol to a JS string 
(internal js/string)

;; Converts an m bool to a JS bool
(internal js/bool)

;; Convert an m natural to a JS number
(internal js/number)

;; Convert an m association list to a JS object
(internal js/object)

;; Test if two JS values are monomorphically equal (===)
(internal js/===?)

;; Test if two JS values are polymorphically equal (==)
(internal js/==?)