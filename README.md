M Language
==========

![travis build](https://travis-ci.com/m-language/m-language.svg?branch=master)

[The M programming language](https://m-language.github.io/) is a minimal Lisp
for the JVM, JS, and Native, designed from the ground up to be as simple as
possible for fast compilation and reusable optimizations.

Getting Started
---------------

See [the getting started guide](https://m-language.readthedocs.io/en/latest/tutorial/starting.html).

What it looks like
----------------------

```lisp
;; This is a comment

;; Definitions are global and unordered
(def list/empty nil)

;; Functions are single argument curried lambdas
(def id (fn x x))
(def const (fn x _ x))

;; Application is also curried
(def swap (fn f x y (f y x)))

;; New forms can be constructed using macros
(macro defn ...)   ; implementation elided
(defn compose f g x (f (g x)))

;; Quotes can be used for special symbols
(def "a name with spaces in it" ...)

;; Pound is syntax sugar for applying a macro to the whole file
#(package demo)

;; Everything after this point is provided as a library
;; and not as part of the language

;; Imports are defined in package.m
#(import nat)

;; Natural numbers are defined in nat.m
(def inc (+ 1))
(def dec ((swap -) 1))

;; Lists are defined in list.m
(defn sum list
  (fold list 0 +))

;; If expressions are macros defined in bool.m
(defnrec factorial x
  (if (= x 0) 1
    (* x (factorial (dec x)))))

;; Processes are used for IO
(defrec echo
  (do
    (x (read-line stdin))
    (_
      (run-async
        (write-line stdout x)
        (write-line (file-out (string "out.log")) x)))
    (_ echo)))

;; Data types are defined in data.m
(defdata point x y)
(def origin (point 0 0))
```

For more information, an in-depth tutorial can be found
[here](https://m-language.readthedocs.io/en/latest/tutorial/index.html),
and the specification can be found
[here](https://github.com/m-language/m-spec/raw/master/m.pdf)
(also available as
[markdown](https://github.com/m-language/m-spec/blob/master/m.md) and
[html](https://m-language.github.io/m-spec/m.html)).

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
please [contact me personally](https://github.com/aedans) and I'll add it to the
organization.

If you want to help M succeed but don't want to contribute code, please consider
starring this repository to help others find it.
