;;; Test.m
;;;
;;; A generic testing framework with test groups and assertions. This is meant
;;; to provide low level testing primitives which can be inspected by tooling,
;;; and to be used a base for more powerful testing frameworks.

;; Creates a successful test.
(def success (left ()))

;; Creates a failed test.
(defn failure messages
  (right messages))

;; Fails a test with a message.
(defn fail message
  (failure (list message)))

;; Combines a list of tests.
(defn combine-tests tests
  (delay
    (fold tests success
      (fn state test
        ((force test) (const state)
          (fn failure1
            (state (delay (failure failure1))
              (fn failure2
                (failure (concat failure1 failure2))))))))))

;; Runs a test.
(defn run-test test
  ((force test)
    (fn success (ostream.writeln stdout (symbol "Tests successful")))
    (fn failure
      (fold failure (impure ())
        (fn process message
          (then-run process
            (ostream.writeln stderr message)))))))

;; Tests that a condition is true.
(defn test-condition condition message
  (if condition success (fail message)))

;; Asserts that a condition is true.
(macrofn assert exprs
  (apply-vararg list-expr0
    (symbol-expr0 (symbol delay))
    (apply-vararg list-expr0
      (symbol-expr0 (symbol test-condition))
      (car exprs)
      (apply-vararg list-expr0
        (symbol-expr0 (symbol symbol))
        (symbol-expr0 (symbol "Assertion failed"))))))

;; Asserts that a condition is true.
(macrofn assert-not exprs
  (apply-vararg list-expr0
    (symbol-expr0 (symbol assert))
    (apply-vararg list-expr0
      (symbol-expr0 (symbol not))
      (car exprs))))