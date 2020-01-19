M Language
==========

**Note: M has recently undergone a complete rewrite, so some things may be
mismatched or missing.**

![travis build](https://travis-ci.com/m-language/m-language.svg?branch=master)

[The M programming language](https://m-language.github.io/) is a minimal 
language, designed from the ground up to be as simple as possible for
readability and expressiveness. 

Getting Started
---------------

See [the getting started guide](https://m-language.readthedocs.io/en/latest/tutorial/starting.html).

What it looks like
------------------

```
# This is a comment

# Definitions are global and unordered
(def one)

# Functions are single argument curried lambdas
(def id (fn x x))
(def const (fn [x y] x))

# Application is also curried
(def swap (fn [f x y] (f y x)))

# New forms can be constructed using macros
(def defn ...)   # implementation elided
(defn (compose f g x) (f (g x)))

# Modules are defined in module.m
(defmodule (demo nat list bool data stdio) {
  # Natural numbers are defined in nat.m
  (def inc (add 1))

  # Lists are defined in list.m
  (defn (sum list)
    (fold list 0 add))

  # Recursion is defined in recur.m
  (defnrec (factorial x)
    ((zero? x) 1
      (mul x (factorial (dec x)))))

  # Processes are used for IO
  (defnrec (echo log)
    (do {
      (def line (read-line stdin))
      (run-async 
        (write-line stdout line)
        (write-line log line))
      (echo log)
    }))

  # Data types are defined in data.m
  (defdata (point x y))
  (def origin (point 0 0))
})
```

For more information, an in-depth tutorial can be found
[here](https://m-language.readthedocs.io/en/latest/tutorial/index.html).

Current State
-------------

M is currently in pre-alpha; some parts of the language and many parts of the
standard library are unimplemented, while others are not heavily tested or are
subject to unhelpful errors. Progress is being made every day, but there is
still a lot of work to be done, so it will be a while before the language is
easily useable.

Contributing
------------

There's a lot of work to be done on M, and any help is greatly appreciated. If
there's a feature you want to add, please open an issue and I'll help however I
can. If you want to add an editor plugin, language backend, or other tooling,
please [contact me](https://github.com/aedans) and I'll add it to the
organization.

If you want to help M succeed but don't want to contribute code, please consider
starring this repository to help others find it.
