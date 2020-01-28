.. _sect-macros:

******
Macros
******

Macros are the core of M's higher level abstractions; they allow you to use
information from the compiler to transform expressions and create new semantics. 
To do this, M treats its own structure as a primitive type, and allows the 
definition of function macros which operate on it.

Macro Application
=================

Macros are applied just like functions, but instead of operating on the values
of their arguments, they operate on the expressions of their arguments.

.. code-block:: lisp

    # The identity function defined using the defn macro
    (defn (id x) x)

Creating Expressions
====================

Quote expressions are of the form ``(quote <expr>)``, where ``expr`` is the 
expression to evaluate to. Quote allows the creation of static expressions which
can be manipulated by other functions.

.. code-block:: lisp

    # The expression representing def
    (quote def)

    # The expression representing (fn x x)
    (quote (fn x x))

Combining Expressions
=====================

Expressions can be combining by applying them to other expressions. The
application of two expressions is equivalent to the expression representing
their application.

.. code-block:: lisp

    # The expression representing (fn x x)
    ((quote fn) qx qx)
    (def qx (quote x))

Transforming Expressions
========================

Function macro expressions are of the form ``(fm <args> <val>)``, where where 
``args`` is a list of argument names and ``val`` is the value of the function
macro. When applied to an expression, a function macro quotes that expression
and transforms it.

.. code-block:: lisp

    # A macro similar to defn
    (def defn-
      (fm [name args value]
        ((quote def) name
          ((quote fn) args value))))
    
    # The identity function defined using the defn- macro
    (defn- id [x] x)

Inspecting Expressions
======================

Case expressions are of the form ``(expr/case <expr> <sym-args> <sym> <nil-args> <nil> <cons-args> <cons>)``.

- When ``expr`` evaluates to a symbol, evaluates to ``sym`` with ``sym-args`` defined as the symbol
- When ``expr`` evaluates to an application with no arguments, evaluates to ``nil`` with ``nil-args`` defined as the function
- When ``expr`` evaluates to an application with arguments, evaluates to ``cons`` with ``cons-args`` defined as the car and cdr

.. code-block:: lisp

    # The implementaion of defn
    (def defn
      (fm [signature value]
        (expr/case signature
          [name] ((quote def) name expr)
          [signature] ((quote defn) signature expr)
          [name args] ((quote def) name ((quote fn) args expr)))))
    
    # In the first case, (defn name value) => (def name value)
    (defn id (fn x x))

    # In the second case, (defn (name) value) => (def name value)
    (defn (id) (fn x x))

    # In the third case, (defn (name args) value) => (def name (fn args value))
    (defn (id x) x)

Currying
========

Internally, function macros are not curried, as they are required to return 
expressions rather than functions. However, they can still be treated like they 
are curried, and will work as expected.

.. code-block:: lisp

    # Defines inc with currying
    ((def inc) (fn x (add 1 x)))

    # Equivalent to the above
    ((defn (inc x)) (add 1 x))
