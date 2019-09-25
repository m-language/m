;; The empty heap.
(def heap/nil (const null))

;; Puts a value in a heap.
(defn heap/put heap name value
  (fn x
    (if (symbol.= x name) (some value) (heap x))))

;; Puts a lazy value into a heap.
(defn heap/put! heap name value!
  (fn x
    (if (symbol.= x name) (some (force value!)) (heap x))))

;; Gets a value in a heap.
(defn heap/get heap name
  (let value? (heap name)
    (if (null? value?)
      (error (concat (symbol->list (symbol "Could not find ")) name))
      (unnull value?))))

;; Concatenates two heaps.
(defn heap/concat heap heap'
  (fn x
    (let value? (heap x)
      (if (null? value?)
        (heap' x)
        value?))))

;; Interpets a tree's definitions.
(defnrec interpret-heap tree heap
  (let type (type-name tree)
       f (pcond (symbol.= type)
           (symbol tree/val) interpret-heap-val
           (symbol tree/def) interpret-heap-def
           (symbol tree/fn) interpret-heap-fn
           (symbol tree/ap) interpret-heap-ap
           (symbol tree/symbol) interpret-heap-symbol
           (error (symbol "...")))
    (f interpret-heap tree heap)))

(defn interpret-heap-val interpret-heap tree heap
  heap)

(defn interpret-heap-def interpret-heap tree heap
  (let heap' (interpret-heap (tree/def.value tree) heap)
    (heap/put! heap'
      (tree/def.name tree)
      (delay (interpret (tree/def.value tree) heap')))))

(defn interpret-heap-fn interpret-heap tree heap
  (interpret-heap (tree/fn.value tree) heap))

(defn interpret-heap-ap interpret-heap tree heap
  (heap/concat
    (interpret-heap (tree/ap.fn tree) heap)
    (interpret-heap (tree/ap.arg tree) heap)))

(defn interpret-heap-symbol interpret-heap tree heap
  heap)

;; Interprets a tree.
(defnrec interpret tree heap
  (let type (type-name tree)
       f (pcond (symbol.= type)
           (symbol tree/val) interpret-val
           (symbol tree/def) interpret-def
           (symbol tree/fn) interpret-fn
           (symbol tree/ap) interpret-ap
           (symbol tree/symbol) interpret-symbol
           (error (symbol "...")))
    (f interpret tree heap)))

(defn interpret-val interpret tree heap
  (heap/get heap (tree/val.name tree)))

(defn interpret-def interpret tree heap
  (heap/get heap (tree/def.name tree)))

(defn interpret-fn interpret tree heap
  (fn arg
    (interpret (tree/fn.value tree)
      (heap/put heap (tree/fn.arg tree) arg))))

(defn interpret-ap interpret tree heap
  ((interpret (tree/ap.fn tree) heap)
    (interpret (tree/ap.arg tree) heap)))

(defn interpret-symbol interpret tree heap
  (tree/symbol.name tree))
