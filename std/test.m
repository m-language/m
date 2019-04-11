;;; Test.m
;;;
;;; A generic testing framework with test groups and assertions. This is meant
;;; to provide low level testing primitives which can be inspected by tooling,
;;; and to be used a base for more powerful testing frameworks.

;; Creates a successful test.
(def success (left ()))

;; Creates a failed test.
(def fail
  (fn message
    (right (list message))))