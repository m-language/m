;;; List.m
;;;
;;; An implementation of lists which are encoded using false as the empty list
;;; and pair as the head and tail of the list.

;; The singleton empty list.
(def nil false)

;; Prepends an element to a list.
(def cons pair)

;; The first element in a list.
(def car first)

;; The rest of the elements in a list.
(def cdr second)

;; The rest of the rest of the list.
(def cddr (compose cdr cdr))

;; The rest of the rest of the rest of the list.
(def cdddr (compose cdr cddr))

;; The second element in a list.
(def cadr (compose car cdr))

;; The third element in a list.
(def caddr (compose car cddr))

;; The fourth element in a list.
(def cadddr (compose car cdddr))

;; Tests if a list is the empty list.
(def nil?
  (fn list
    (list
      (const (const (const false)))
      true)))

;; Creates a literal list.
(macro list
  (fn env exprs
    (result/success
      (((nil? exprs) (const (expr/symbol (symbol nil)))
        (fn ""
          (expr/list
            (cons (expr/symbol (symbol cons))
            (cons (car exprs)
            (cons (expr/list (cons (expr/symbol (symbol list)) (cdr exprs)))
              nil)))))) nil))))

;; The initial elements of a list.
(defnrec init list
  (if (nil? (cdr list)) nil
    (cons (car list) (init (cdr list)))))

;; The last element of a list.
(defnrec last list
  (if (nil? (cdr list))
    (car list)
    (last (cdr list))))

;; Appends an element to a list.
(defnrec append list elem
  (if (nil? list)
    (cons elem nil)
    (cons (car list) (append (cdr list) elem))))

;; Concatenates two lists.
(defnrec concat a b
  (if (nil? a) b
    (cons (car a) (concat (cdr a) b))))

;; Gets the nth element of a list.
(defnrec get list n
  (if (nat.0? n)
    (car list)
    (get (cdr list) (nat.dec n))))

;; Maps a list with a function.
(defnrec map list f
  (if (nil? list) nil
    (cons (f (car list)) (map (cdr list) f))))

;; Flat maps a list with a function.
(defnrec flat-map list f
  (if (nil? list) nil
    (concat (f (car list)) (flat-map (cdr list) f))))

;; Filters a list with a function.
(defnrec filter list f
  (if (nil? list) nil
    (if (f (car list))
      (cons (car list) (filter (cdr list) f))
      (filter (cdr list) f))))

;; Folds a list with an accumulator and a function.
(defnrec fold list acc f
  (if (nil? list) acc
    (fold (cdr list) (f acc (car list)) f)))

;; Implementation of reverse.
(defnrec reverse' list acc
  (if (nil? list) acc
    (reverse'
      (cdr list)
      (cons (car list) acc))))

;; Reverses a list.
(defn reverse list
  (reverse' list nil))

;; Tests if two lists are equal are equal given its element's equality function.
(defnrec list.= = a b
  (cond
    (nil? a) (nil? b)
    (nil? b) false
    (& (= (car a) (car b))
       (list.= = (cdr a) (cdr b)))))

;; Compares two lists given a compare function.
(defnrec compare-list compare a b
  (cond
    (& (nil? a) (nil? b)) compare=
    (nil? a) compare<
    (nil? b) compare>
    (let compare-result (compare (car a) (car b))
      (if (compare=? compare-result)
        (compare-list compare (cdr a) (cdr b))
        compare-result))))
