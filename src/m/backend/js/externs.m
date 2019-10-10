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