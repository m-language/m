;;; Test.m
;;;
;;; A generic testing framework with test groups and assertions. This is meant
;;; to provide low level testing primitives which can be inspected by tooling,
;;; and to be used a base for more powerful testing frameworks.

;; Creates a successful test.
(def success (left ()))

;; Creates a failed test.
(def fail
  (fn messages
    (right messages)))

;; Runs a list of tests.
(def run-tests
  (fn tests _
    ((fold tests success
      (fn state test
        ((force test) id
          (fn fail1
            (state (const fail1)
              (fn fail2
                (fail (concat fail1 fail2)))))))))))