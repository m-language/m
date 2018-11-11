;; The singleton empty list.
(def nil ())

;; Prepends an element to a list.
(def cons ())

;; The first element in a list.
(def car ())

;; The rest of the elements in a list.
(def cdr ())

(include symbol)

;; Tests if a value is the empty list.
(def is-nil
  (lambda x
    (eq-symbol (type-name x) (symbol nil))))

(include function)

;; The second element in a list.
(def cadr (compose car cdr))

;; The rest of the rest of the list.
(def cddr (compose cdr cdr))

;; The third element in a list.
(def caddr (compose car cddr))

;; The fourth element in a list.
(def cadddr (compose car (compose cdr cddr)))

;; Appends [elem] to [list].
(def append
  (lambda list
    (lambda elem
      (if (is-nil list)
        (cons elem list)
        (cons (car list) (append (cdr list) elem))))))

;; Adds [list1] and [list2].
(def add-list
  (lambda list1
    (lambda list2
      (if (is-nil list1)
        list2
        (cons (car list1) (add-list (cdr list1) list2))))))

;; Maps [list] with function [f].
(def map
  (lambda list
    (lambda f
      (if (is-nil list)
        nil
        (cons (f (car list)) (map (cdr list) f))))))

;; Folds [list] with an accumulator [acc] function [f].
(def fold
  (lambda list
    (lambda acc
      (lambda f
        (if (is-nil list)
          acc
          (fold (cdr list) (f acc (car list)) f))))))

;; Takes elements of [list] while [f] is true.
(def take-while
  (lambda list
    (lambda f
      (if (is-nil list)
        nil
        (if (f (car list))
          (cons (car list) (take-while (cdr list) f))
          nil)))))

(include bool)

;; Tests if [list1] and [list2] are equal given a function [f].
(def eq-list
  (lambda f
    (lambda list1
      (lambda list2
        (if (is-nil list1)
          (is-nil list2)
          (if (is-nil list2)
            false
            (and (f (car list1) (car list2))
                 (lambda unused (eq-list f (cdr list1) (cdr list2))))))))))