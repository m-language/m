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
  (lambda list
    (list
      (const (const (const false)))
      true)))

;; Creates a list with a single element.
(def list1
  (lambda a
    (cons a ())))

;; Creates a list with two elements.
(def list2
  (lambda a
    (lambda b
      (cons a (list1 b)))))

;; Creates a list with three elements.
(def list3
  (lambda a
    (lambda b
      (lambda c
        (cons a (list2 b c))))))

;; Creates a list with four elements.
(def list4
  (lambda a
    (lambda b
      (lambda c
        (lambda d
          (cons a (list3 b c d)))))))

;; Creates a list with five elements.
(def list5
  (lambda a
    (lambda b
      (lambda c
        (lambda d
          (lambda e
            (cons a (list4 b c d e))))))))

;; Creates a list with six elements.
(def list6
  (lambda a
    (lambda b
      (lambda c
        (lambda d
          (lambda e
            (lambda f
              (cons a (list5 b c d e f)))))))))

;; Appends an element to a list.
(def append
  (lambda list
    (lambda elem
      (if (nil? list)
        (list1 elem)
        (cons (car list) (append (cdr list) elem))))))

;; Concatenates two lists.
(def concat
  (lambda list1
    (lambda list2
      (if (nil? list1)
        list2
        (cons (car list1) (concat (cdr list1) list2))))))

;; Gets the nth element of a list.
(def get
  (lambda list
    (lambda n
      (if (nat.0? n)
        (car list)
        (get (cdr list) (nat.dec n))))))

;; Maps a list with a function.
(def map
  (lambda list
    (lambda f
      (if (nil? list)
        ()
        (cons (f (car list)) (map (cdr list) f))))))

;; Flat maps a list with a function.
(def flat-map
  (lambda list
    (lambda f
      (if (nil? list)
        ()
        (append (f (car list)) (flat-map (cdr list) f))))))

;; Folds a list with an accumulator and a function.
(def fold
  (lambda list
    (lambda acc
      (lambda f
        (if (nil? list)
          acc
          (fold (cdr list) (f acc (car list)) f))))))

;; Implementation of reverse.
(def reverse'
  (lambda list
    (lambda acc
      (if (nil? list)
        acc
        (reverse'
          (cdr list)
          (cons (car list) acc))))))

;; Reverses a list.
(def reverse
  (lambda list
    (reverse' list ())))

;; Tests if two lists are equal are equal given its element's equality function.
(def list.=
  (lambda f
    (lambda list1
      (lambda list2
        (if (nil? list1)
          (nil? list2)
          (if (nil? list2)
            false
            (and (f (car list1) (car list2))
                 (lambda "" (list.= f (cdr list1) (cdr list2))))))))))