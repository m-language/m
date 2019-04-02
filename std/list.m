;;; List.m
;;;
;;; An implementation of lists which are encoded using false as the empty list
;;; and pair as the head and tail of the list.
;;;
;;; All definitions in this file are optimized to use the backend's native
;;; implementation of lists.

;; The singleton empty list.
(def nil ())

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

;; Creates a list with a single element.
(def list1
  (fn a
    (cons a ())))

;; Creates a list with two elements.
(def list2
  (fn a b
    (cons a (list1 b))))

;; Creates a list with three elements.
(def list3
  (fn a b c
    (cons a (list2 b c))))

;; Creates a list with four elements.
(def list4
  (fn a b c d
    (cons a (list3 b c d))))

;; Creates a list with five elements.
(def list5
  (fn a b c d e
    (cons a (list4 b c d e))))

;; Creates a list with six elements.
(def list6
  (fn a b c d e f
    (cons a (list5 b c d e f))))

;; The initial elements of a list.
(def init
  (fn list
    (if (nil? (cdr list)) ()
      (cons (car list) (init (cdr list))))))

;; The last element of a list.
(def last
  (fn list
    (if (nil? (cdr list))
      (car list)
      (last (cdr list)))))

;; Appends an element to a list.
(def append
  (fn list elem
    (if (nil? list)
      (list1 elem)
      (cons (car list) (append (cdr list) elem)))))

;; Concatenates two lists.
(def concat
  (fn a b
    (if (nil? a) b
      (cons (car a) (concat (cdr a) b)))))

;; Gets the nth element of a list.
(def get
  (fn list n
    (if (nat.0? n)
      (car list)
      (get (cdr list) (nat.dec n)))))

;; Maps a list with a function.
(def map
  (fn list f
    (if (nil? list) ()
      (cons (f (car list)) (map (cdr list) f)))))

;; Flat maps a list with a function.
(def flat-map
  (fn list f
    (if (nil? list) ()
      (concat (f (car list)) (flat-map (cdr list) f)))))

;; Filters a list with a function.
(def filter
  (fn list f
    (if (nil? list) ()
      (if (f (car list))
        (cons (car list) (filter (cdr list) f))
        (filter (cdr list) f)))))

;; Folds a list with an accumulator and a function.
(def fold
  (fn list acc f
    (if (nil? list) acc
      (fold (cdr list) (f acc (car list)) f))))

;; Implementation of reverse.
(def reverse'
  (fn list acc
    (if (nil? list) acc
      (reverse'
        (cdr list)
        (cons (car list) acc)))))

;; Reverses a list.
(def reverse
  (fn list
    (reverse' list ())))

;; Tests if two lists are equal are equal given its element's equality function.
(def list.=
  (fn f a b
    (if (nil? a) (nil? b)
    (if (nil? b) false
      (& (f (car a) (car b))
         (list.= f (cdr a) (cdr b)))))))