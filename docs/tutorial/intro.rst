.. _sect-intro:

************
Introduction
************

The fundamental philosophy of the M programming language is that simplicity is
as important as expressiveness; or, put another way, that a program which cannot
be reasoned about is barely better than no program at all. Many features of
modern programming languages complicate programs unnecessarily, and combining
these features increases this complexity exponentially. For the programmer, this
means time wasted trying to figure out what a program even does (or worse, being
wrong about what a program does). For the tooling, this means less support,
worse performance, and more bugs.

M attempts to replace these complicated language features with primitive
abstractions, making programs simpler and easier to reason about. While many
of these features are still available in M, they are provided through the
composition of primitive abstractions rather than internal tooling support,
allowing them to be programmatically modified or erased. Looking at the
implementation of a feature is as simple as looking at the implementation of a
function, and removing it is as simple as inlining said function.

Despite this emphasis on simplicity, M maintains (and even exceeds) the
expressiveness of most other programming languages. The ability to define new
features in M makes adding them easy, and the tooling doesn't need to be updated
to support them. Slight changes in features can be used along side the
originals, and complete paradigm switches can be made local to a single
function.

Writing this tutorial was difficult at first because M is so simple; I was able
to fit the entire definition into a document barely larger than this
introduction, but it felt like a list of random rules rather than an actual
programming language. Because of this, I've instead decided to cover the M
standard library, explaining the ways M's systems interact with one another,
and eventually culminating in a complete implementation of a self-interpreter.

If you just want a quick introduction to the semantics of the language,
the `reference <../reference.html>`_ covers everything you need to know, while
`The M Specification <https://github.com/m-language/m-spec/raw/master/m.pdf>`_
provides the formal syntax and semantics.

Otherwise, `continue on <starting.html>`_.

Intended Audience
=================

This tutorial is intended as a simple introduction to M, and while it does not
assume any prior knowledge, familiarity the Lambda Calculus, other Lisps, and
functional programming in general will be useful. Though many of the topics
covered are relatively simple, the ways in which they combine can be difficult
to understand if you are not already familiar with them. Every concept will be
explained at least briefly, but fully exploring concepts like monads and macros
is beyond the scope of this tutorial.
