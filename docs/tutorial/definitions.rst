.. _sect-definitions:

***********
Definitions
***********

While it is possible to introduce local variables with functions, something more
powerful is needed to abstract larger programs. M's definitions have simple
compositional semantics which allow them to to easily be extended.

Definition expressions are of the form ``(def <name> <value>)``, where name is
the name of the definition and value is is the value of the definition.

.. code-block:: lisp

    ;; The identity function which always returns its argument.
    (def id (fn x x))

    ;; The constant function which ignores its second argument.
    (def const (fn x _ x))

Name Collisions
===============

M does not allow name collisions and fails as soon as a definition with a name
that has already been defined is introduced.

.. code-block:: lisp

    ;; 1
    (def one (nat 1))

    ;; Error: one has already been defined.
    (def one (inc (nat 0)))

Ordering
========

M definitions can be put in any order regardless of their dependencies; the M
compiler will order the definitions such that a definition is always put before
its first usage.

.. code-block:: lisp

    ;; 3
    (inc (nat 2))

    ;; The increment function which adds one to its argument.
    (def inc (+ 1))

Recursion
=========

M does not natively support recursion; it is instead emulated through the fixed
point combinator as shown later in `recursion <recursion.html>`_.

.. code-block:: lisp

    ;; Error: could not find recursive.
    (def recursive (fn x (recursive x)))

As Expressions
==============

Definitions are expressions which return the value of what they define. They
can be treated as though they have been lifted from their context to the top
level and replaced by the identifier which refers to them.

.. code-block:: lisp

    ;; 3
    ((def inc (+ 1))
     (nat 2))

    ;; Equivalent to the above.
    (def inc (+ 1))
    (inc (nat 2))

    ;; Error: could not find x.
    (fn x
      (def invalid (inc x)))

    ;; Equivalent to the above.
    (def invalid (inc x))
    (fn x invalid)

Cross File Definitions
======================

M's definitions are designed in such a way that two definitions in two different
files are equivalent to the same two definitions in the same file. In other
words, it does not matter where the definitions are stored as long as the M
compiler can find them.

By default, the M compiler looks in the current directory and in MPM for
definitions. This is semantically equivalent to combining every file in your
project and every file in MPM into one file, though optimizations are in place
to avoid compiling unused files. This functionality also extends to the REPL.
