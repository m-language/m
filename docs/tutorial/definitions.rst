.. _sect-definitions:

***********
Definitions
***********

While it is possible to introduce local variables with functions, something more
powerful is needed to abstract in larger programs. Definitions provide a simple
way of providing both local and global variables without worrying about includes 
or forward declarations.

Definition Expressions
======================

Definition expressions are of the form ``(def <name> <val>)``, where ``name`` is
the name of the definition and ``val`` is is the value of the definition.

.. code-block:: lisp

    # The identity function which always returns its argument.
    (def id (fn x x))

    # The constant function which ignores its second argument.
    (def const (fn [x y] x))

Ordering
========

M definitions can be put in any order regardless of their dependencies; the M
compiler will order the definitions such that a definition is always put before
its first usage.

.. code-block:: lisp

    # 3
    (def three (inc 2))

    # The increment function which adds one to its argument
    (def inc (add 1))

Applying Definitions
====================

Definitions are expressions which return an environment. When an environment is
applied, it evaluates its argument within the environment.

.. code-block:: lisp

    # 3
    ((def inc (add 1))
      (inc 2))

    # Equivalent to the above due to currying
    (def inc (add 1)
      (inc 2))

Block Expressions
=================

Block expressions are expressions of the form ``(block exprs)``, where ``exprs``
is the list of expressions in the block. A block expression combines the
environments of all expressions in the block to create a single environment.

.. code-block:: lisp

    # Defines three and inc
    (block {
      (def three (inc 2))
      (def inc (add 1))
    })

Semantically, the top level of an M program is a block expression.

Cross File Definitions
======================

M's definitions are designed in such a way that two definitions in two different
files are equivalent to two definitions in the same file. In other words, it 
does not matter where the definitions are stored as long as the M compiler can 
find them.

By default, the M compiler looks in the current directory for definitions. This 
is semantically equivalent to combining each file in your project into one 
file.
