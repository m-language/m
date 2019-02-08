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
(def car left)

;; The rest of the elements in a list.
(def cdr right)

;; The rest of the rest of the list.
(def cddr (ap compose cdr cdr))

;; The rest of the rest of the rest of the list.
(def cdddr (ap compose cdr cddr))

;; The second element in a list.
(def cadr (ap compose car cdr))

;; The third element in a list.
(def caddr (ap compose car cddr))

;; The fourth element in a list.
(def cadddr (ap compose car cdddr))

;; Tests if a list is the empty list.
(def nil?
  (fn list
    (ap list
      (ap const (ap const (ap const false)))
      true)))

;; Creates a list with a single element.
(def list1
  (fn a
    (ap cons a ())))

;; Creates a list with two elements.
(def list2
  (fn a
    (fn b
      (ap cons a (ap list1 b)))))

;; Creates a list with three elements.
(def list3
  (fn a
    (fn b
      (fn c
        (ap cons a (ap list2 b c))))))

;; Creates a list with four elements.
(def list4
  (fn a
    (fn b
      (fn c
        (fn d
          (ap cons a (ap list3 b c d)))))))

;; Creates a list with five elements.
(def list5
  (fn a
    (fn b
      (fn c
        (fn d
          (fn e
            (ap cons a (ap list4 b c d e))))))))

;; Creates a list with six elements.
(def list6
  (fn a
    (fn b
      (fn c
        (fn d
          (fn e
            (fn f
              (ap cons a (ap list5 b c d e f)))))))))

;; Appends an element to a list.
(def append
  (fn list
    (fn elem
      (if (ap nil? list)
        (ap list1 elem)
        (ap cons (ap car list) (ap append (ap cdr list) elem))))))

;; Concatenates two lists.
(def concat
  (fn list1
    (fn list2
      (if (ap nil? list1)
        list2
        (ap cons (ap car list1) (ap concat (ap cdr list1) list2))))))

;; Gets the nth element of a list.
(def get
  (fn list
    (fn n
      (if (ap nat.0? n)
        (ap car list)
        (ap get (ap cdr list) (ap nat.dec n))))))

;; Maps a list with a function.
(def map
  (fn list
    (fn f
      (if (ap nil? list)
        ()
        (ap cons (ap f (ap car list)) (ap map (ap cdr list) f))))))

;; Flat maps a list with a function.
(def flat-map
  (fn list
    (fn f
      (if (ap nil? list)
        ()
        (ap append (ap f (ap car list)) (ap flat-map (ap cdr list) f))))))

;; Folds a list with an accumulator and a function.
(def fold
  (fn list
    (fn acc
      (fn f
        (if (ap nil? list)
          acc
          (ap fold (ap cdr list) (ap f acc (ap car list)) f))))))

;; Implementation of reverse.
(def reverse'
  (fn list
    (fn acc
      (if (ap nil? list)
        acc
        (ap reverse'
          (ap cdr list)
          (ap cons (ap car list) acc))))))

;; Reverses a list.
(def reverse
  (fn list
    (ap reverse' list ())))

;; Tests if two lists are equal are equal given its element's equality function.
(def list.=
  (fn f
    (fn list1
      (fn list2
        (if (ap nil? list1)
          (ap nil? list2)
          (if (ap nil? list2)
            false
            (ap and (ap f (ap car list1) (ap car list2))
                 (fn "" (ap list.= f (ap cdr list1) (ap cdr list2))))))))))