.. _sect-intro:

************
Introduction
************

The fundamental idea behind the M programming language is that simplicity is as 
important as expressiveness; or, put another way, that a program which cannot
be reasoned about is little better than no program at all. Many features of
modern programming languages complicate programs unnecessarily, and combining
these features increases complexity exponentially. For the programmer, this
means time wasted trying to figure out what a program does. For the tooling, 
this means less support, worse performance, and more bugs.

M attempts to replace these complicated language features with straightforward
abstractions, making programs simpler and easier to reason about. While many
advanced features are still available in M, they are provided through the
composition of abstractions rather than as part of the language, allowing them 
to be programmatically modified or erased. Looking at the implementation of a 
feature is as simple as looking at the implementation of a function, and
removing it is as simple as inlining said function.

Despite this emphasis on simplicity, M maintains (and even exceeds) the
expressiveness of most other programming languages. The ability to define new
features in M makes adding missing ones simple, and no tooling needs to be
updated to support them. Modifications of features can be used along side one
another, and complete paradigm switches can be made local to a single function.

If you just want a quick introduction to the semantics of the language,
the `reference <../reference.html>`_ covers everything you need to know.
Otherwise, `continue on <starting.html>`_.

Intended Audience
=================

This tutorial is intended as a simple introduction to M, and while it does not
assume any prior knowledge, familiarity the Lambda Calculus, other Lisps, and
functional programming in general will be useful. Though many of the topics
covered are relatively simple, the ways in which they interact can be difficult
to understand if you are not already familiar with them. Every concept will be
explained at least briefly, but fully exploring concepts like monads is beyond 
the scope of this tutorial.
