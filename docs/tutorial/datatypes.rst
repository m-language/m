.. _sect-datatypes:

**********
Data Types
**********

M does not have an intrinsic way to create data types. Instead, data types are
created with functions that use closures to store information. Behind the
scenes, all data types described here use native machine representations, but
the properties exposed by the function representation can still be used.

Booleans
========

Booleans are a data type representing a choice; the boolean true represents one
option, while the boolean false represents the other option. These are
represented as a function which ignores its first or second argument.

.. code-block:: lisp

    ;; The singleton truthy value.
    (def true (fn x _ x))

    ;; The singleton falsy value.
    (def false (fn _ x x))

    ;; a
    (true a b)

    ;; b
    (false a b)

Naturals
========

Natural numbers are a data type representing repetition, and are represented
as a function which applies a function to an argument N times, where N is the
natural number.

.. code-block:: lisp

    ;; The smallest natural number.
    (def zero (fn f x x))

    ;; The unit natural number.
    (def one (fn f x (f x)))

    ;; Increments a number.
    (def inc
      (fn n
        (fn f x
          (f (n f x)))))

Products
========

Products, known more commonly as pairs, are a data type which represents the
combination of two data types, one of which can be a product of its own. It is
used to create larger data types by composition, and can be represented as a
function from a boolean to a value.

.. code-block:: lisp

    ;; Creates a pair of two values.
    (def pair
      (fn first second
        (fn bool
          (bool first second))))

    ;; The first value of a pair.
    (def first
      (fn pair
        (pair true)))

    ;; The second value of a pair.
    (def second
      (fn pair
        (pair false)))

Coproducts
==========

Coproducts, known more commonly as either, are the opposite of products; they
represent the choice between two data types, one of which can be a coproduct of
its own. It is used to create larger data types by composition, and can be
represented as a function which applies a function to its choice.

.. code-block:: lisp

    ;; Creates a left either.
    (def left
      (fn value
        (fn f _
          (f value))))

    ;; Creates a right either.
    (def right
      (fn value
        (fn _ f
          (f value))))

    ;; Folds an either into a value.
    (def fold
      (fn either left right
        (either
          (fn _ left)
          (fn _ right))))

Others
======

Other data types can be represented as the composition of these data types. Here
are some examples:

- char = nat
- int = pair bool nat
- option a = either a bool
- list a = either bool (pair a (list a))
- string = list char
- struct = pair string (string -> any)
