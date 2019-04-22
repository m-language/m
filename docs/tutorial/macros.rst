.. _sect-macros:

******
Macros
******

Macros are the core of M's higher level abstractions; they allow you to use
information from the compiler to modify expressions before runtime and create
new semantics. Many features in this tutorial so far have actually been macros;
for example, the ``nat`` macro, which turns a natural number into the expression
required to create it. Later in this tutorial more advanced macros will be
explored which allow things like data types, type systems, and more.

Macro Application
=================

Macros are applied just like functions, but instead of operating on the values
of their arguments, they operate on the expressions of their arguments. 

.. code-block:: lisp

    ;; The application of the nat macro to the symbol 1
    (nat 1)

    ;; A function which ignores its argument and returns long-computation lazily
    (delay long-computation)

Expressions
===========

M expressions are either symbol expressions or list expressions. Both are
represented with their respective data types, but cannot be operated on 
directly; instead there are several internal macros for operating on them.

- ``macro/list`` creates a list expression given a list of expressions.
- ``macro/symbol`` creates a symbol expression given a symbol.
- ``macro/match`` takes an expression and two functions, applies the first if the
  the expression is a symbol, and applies the second if it is a list.

Macro Definition
================

Macro definitions are of the form ``(macro <name> <value>)``, where name is the
name of the macro and value is the value of the macro. 

.. code-block:: lisp

    ;; Macro which delays the evaluation of an expression
    (macro delay
      (fn exprs
        (macro/list
          (cons (macro/symbol (symbol fn))
          (cons (macro/symbol (symbol _))
            exprs)))))
