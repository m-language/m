.. _sect-definitions:

***********
Definitions
***********

While it is possible to introduce local variables with functions, something more
powerful is needed to abstract in larger programs. M's definitions have simple
compositional semantics which allow them to to easily be extended.

Definition expressions are of the form ``(def <name> <val>)``, where ``name`` is
the name of the definition and ``val`` is is the value of the definition.

.. code-block:: lisp

    # The identity function which always returns its argument.
    (def id (fn x x))

    # The constant function which ignores its second argument.
    (def const (fn [x y] x))

Name Collisions
===============

M does not allow name collisions and fails as soon as a definition with a name
that has already been defined is introduced.

.. code-block:: lisp

    # 1
    (def one 1)

    # Error: one has already been defined
    (def one (inc 0))

Ordering
========

M definitions can be put in any order regardless of their dependencies; the M
compiler will order the definitions such that a definition is always put before
its first usage.

.. code-block:: lisp

    # 3
    (inc 2)

    # The increment function which adds one to its argument
    (def inc (add 1))

Recursion
=========

M does not natively support recursion; it is instead emulated through the fixed
point combinator as shown later in `recursion <recursion.html>`_.

.. code-block:: lisp

    # Error: could not find recursive
    (def recursive (fn x (recursive x)))

As Expressions
==============

Definitions are expressions which return the value of what they define. Since
they are expressions, they can also be used in functions and passed as 
arguments.

.. code-block:: lisp

    # 3
    ((def inc (add 1)) 2)

    # Equivalent to the above
    (def inc (add 1))
    (inc 2)

    # Equivalent to the above
    (inc 2)
    ((fn one (def inc (add one)))
      1)

Cross File Definitions
======================

M's definitions are designed in such a way that two definitions in two different
files are equivalent to the same two definitions in the same file. In other
words, it does not matter where the definitions are stored as long as the M
compiler can find them.

By default, the M compiler looks in the current directory for definitions. This 
is semantically equivalent to combining every file in your project into one 
file, though optimizations are in place to avoid compiling unused files. This 
functionality also extends to the REPL.
