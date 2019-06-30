;;; Test.m
;;;
;;; A generic testing framework with test groups and assertions. This is meant
;;; to provide low level testing primitives which can be inspected by tooling,
;;; and to be used a base for more powerful testing frameworks.

;; Creates a successful test.
(def success (left nil))

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
      (fold failure (impure nil)
        (fn process message
          (then-run process
            (ostream.writeln stderr message)))))))

;; Tests that a condition is true.
(defn test-condition condition message
  (if condition success (fail message)))

;; Asserts that a condition is true.
(macrofn assert env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol delay))
      (apply-vararg expr/list
        (expr/symbol (symbol test-condition))
        (car exprs)
        (apply-vararg expr/list
          (expr/symbol (symbol symbol))
          (expr/symbol (symbol "Assertion failed")))))))

;; Asserts that a condition is true.
(macrofn assert-not env exprs
  (result/success
    (apply-vararg expr/list
      (expr/symbol (symbol assert))
      (apply-vararg expr/list
        (expr/symbol (symbol not))
        (car exprs)))))