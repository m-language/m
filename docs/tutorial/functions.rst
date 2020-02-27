.. _sect-functions:

*********
Functions
*********

Functions are the fundamental data type in M. Every type, except for a few
primitives such as integers and expressions, is represented as a function, 
even typically intrinsic types like structures and booleans. Functions in M are 
pure, meaning that they cannot have side effects like IO or mutation, and only 
ever take one argument (multi-argument functions are provided as syntax sugar).

Function Application
====================

Function application is of the form ``(<fn> <args...>)``, where ``fn`` is the 
function to be applied and ``args`` is a list of arguments to the function.

.. code-block:: lisp

    # True
    (not false)

    # 4
    (add 1 3)

Functions are applied lazily, so their arguments are only evaluated when used.

Function Abstraction
====================

Function abstraction is of the form ``(fn <args> <val>)``, where ``args`` is 
a list of argument names and ``val`` is the value of the function.

.. code-block:: lisp

    # The identity function which always returns its argument
    (fn x x)

    # The constant function which ignores its second argument
    (fn [x y] x)

    # The increment function
    (fn x (add x 1))

Note that the syntaxes ``(f x)``, ``[f x]``, and ``{f x}`` are interchangeable;
parentheses are used when applying functions, brackets are used when passing a 
list of expressions which will not be evaluated, and braces are used when 
passing a list of expressions which will be evaluated.

Closures
========

Closures are a property of functions where the variables of an outside scope are
captured by a function. The values of these variables are stored in the
function's closure, and persists for as long as the function persists.

.. code-block:: lisp

    # The increment function expressed with closures; the variable x is bound
    # to 1, and the function of y stores this value in its closure
    ((fn x (fn y (add x y))) 
     1)

Currying
========

Curried functions are functions which use closures to take their arguments one
at a time, allowing for partial application of their arguments. In M, all
functions are curried, including internal functions.

.. code-block:: lisp

    # This has the same effect as the example above due to currying.
    (add 1)

Multi-Argument Functions
========================

Multi-argument functions in M are actually just syntax sugar for curried
functions. Likewise, multi-argument application is just syntax sugar for 
curried application.

.. code-block:: lisp

    # The two-argument application function with syntax sugar.
    (fn [x y z] (x y z))

    # The two-argument application function without syntax sugar.
    (fn x (fn y (fn x ((x y) z))))
