;;; Test.m
;;;
;;; A generic testing framework with test groups and assertions. This is meant
;;; to provide low level testing primitives which can be inspected by tooling,
;;; and to be used a base for more powerful testing frameworks.

;; Creates a successful test.
(def success (left ()))

;; Creates a failed test.
(def failure
  (fn messages
    (right messages)))

;; Fails a test with a message.
(def fail
  (fn message
    (failure (list message))))

;; Combines a list of tests.
(def combine-tests
  (fn tests
    (delay
      (fold tests success
        (fn state test
          ((force test) (const state)
            (fn failure1
              (state (delay (failure failure1))
                (fn failure2
                  (failure (concat failure1 failure2)))))))))))

;; Runs a test.
(def run-test
  (fn test
    ((force test)
      (fn success (ostream.writeln stdout (symbol "Tests successful")))
      (fn failure
        (fold failure (impure ())
          (fn process message
            (then-run process
              (ostream.writeln stderr message))))))))

;; Tests that a condition is true.
(def test-condition
  (fn condition message
    (if condition success (fail message))))

;; Asserts that a condition is true.
(macro assert
  (fn expr
    (apply-vararg expr.list
      (expr.symbol (symbol delay))
      (apply-vararg expr.list
        (expr.symbol (symbol test-condition))
        (car expr)
        (apply-vararg expr.list
          (expr.symbol (symbol symbol))
          (expr.symbol (symbol "Assertion failed")))))))

;; Asserts that a condition is true.
(macro assert-not
  (fn expr
    (apply-vararg expr.list
      (expr.symbol (symbol assert))
      (apply-vararg expr.list
        (expr.symbol (symbol not))
        (car expr)))))