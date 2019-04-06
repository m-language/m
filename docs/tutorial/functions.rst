.. _sect-functions:

*********
Functions
*********

Unlike most programming languages, functions are completely fundamental to M.
Every data type in M is represented as a function, even typically intrinsic ones
like structs and integers. These representations will be covered in
`data types <data.html>`_, but for now it is enough to know that functions are
the singular object in M.

Functions in M are pure, meaning that they return the same result for the same
input, and only ever take one argument (multi-argument functions are provided as
syntax sugar).

Function expressions are of the form ``(fn <arguments...> <value>)``, where
arguments is a list of argument names and value is the value of the function.

.. code-block:: lisp

    ;; The identity function, a function which always returns its argument.
    (fn x x)

    ;; The constant function, a function which ignores its second argument.
    (fn x _ x)

Application
===========

Of course, functions themselves do nothing; application is needed to achieve
any computation. Application expressions are of the form
``(<function> <arguments...>)``, where function is the function to be applied
and arguments is a list of arguments to the function.

.. code-block:: lisp

    ;; True
    (not false)

    ;; False
    (and true false)

Functions are applied eagerly, so their arguments are always evaluated before
the function, but an argument evaluation order is not specified.

Closures
========

Closures are a property of functions where the variables of an outside scope are
captured by a function. The values of these variables are stored in the
function's closure, and persists as long as the function persists.

.. code-block:: lisp

    ;; The increment function expressed with closures; the variable x is bound
    ;; to one, and the function of y stores this value in its closure.
    ((fn x
       (fn y
         (+ x y)))
     (nat 1))

Currying
========

Curried functions are functions which use closures to take their arguments one
at a time, allowing for partial application of their arguments. In M, all
functions are curried.

.. code-block:: lisp

    ;; This has the same effect as the example above since addition is curried.
    (+ (nat 1))

Multi-Argument Functions
========================

As you might have guessed, multi-argument functions in M are actually just
syntax sugar for curried functions. Likewise, multi-argument application is just
repeated application of a single argument.

.. code-block:: lisp

    ;; The two-argument application function with syntax sugar.
    (fn x y z (x y z))

    ;; The two-argument application function without syntax sugar.
    (fn x (fn y (fn x ((x y) z))))
