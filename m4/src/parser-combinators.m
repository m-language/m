(import predef)

;; A parse result representing failure.
(def parse-failure
  (lambda state
    (derive (symbol parse-failure) (symbol state) state
      (object (symbol parse-failure)))))

(def parse-failure.state (field (symbol parse-failure) (symbol state)))

;; A parse result representing success.
(def parse-success
  (lambda value
    (lambda state
      (lambda rest
        (derive (symbol parse-success) (symbol value) value
        (derive (symbol parse-success) (symbol state) state
        (derive (symbol parse-success) (symbol rest) rest
          (object (symbol parse-success)))))))))

(def parse-success.value (field (symbol parse-success) (symbol value)))
(def parse-success.state (field (symbol parse-success) (symbol state)))
(def parse-success.rest (field (symbol parse-success) (symbol rest)))

;; Tests if a value is a parse success.
(def is-parse-success
  (lambda x
    (eq-symbol (type-name x) (symbol parse-success))))

;; A parser which succeeds only if [f] of the next element is true.
(def predicate-parser
  (lambda f
    (lambda input
      (lambda state
        (if (and (not (is-nil input))
                 (lambda unused (f (car input))))
          (parse-success (car input) state (cdr input))
          (parse-failure state))))))

;; A parser which always succeeds.
(def success-parser (predicate-parser (lambda unused true)))

;; Maps [parser]'s result with the function [f].
(def map-parser
  (lambda parser
    (lambda f
      (lambda input
        (lambda state
          (f (parser input state)))))))

;; Maps [parser]'s result with the function [f] if the result is a success.
(def map-parser-success
  (lambda parser
    (lambda f
      (map-parser parser
        (lambda result
          (if (is-parse-success result)
            (f result)
            result))))))

;; Maps [parser]'s result's value with the function [f].
(def map-parser-value
  (lambda parser
    (lambda f
      (map-parser-success parser
        (lambda success
          (parse-success
            (f (parse-success.value success))
            (parse-success.state success)
            (parse-success.rest success)))))))

;; Maps [parser]'s result's state with the function [f].
(def map-parser-state
  (lambda parser
    (lambda f
      (map-parser-success parser
        (lambda success
          (parse-success
            (parse-success.value success)
            (f (parse-success.state success))
            (parse-success.rest success)))))))

;; Provides the state before [parser] was run.
(def provide-past-state
  (lambda parser
    (lambda input
      (lambda state
        ((map-parser-value parser
          (lambda value
            (pair value state)))
        input state)))))

;; Combines [parser1] and [parser2].
(def combine-parser
  (lambda parser1
    (lambda parser2
      (lambda input
        (lambda state
          ((lambda parser1-result
            (if (is-parse-success parser1-result)
              ((lambda parser2-result
                (if (is-parse-success parser2-result)
                  (parse-success
                    (pair
                      (parse-success.value parser1-result)
                      (parse-success.value parser2-result))
                    (parse-success.state parser2-result)
                    (parse-success.rest parser2-result))
                  parser2-result))
              (parser2
                (parse-success.rest parser1-result)
                (parse-success.state parser1-result)))
              parser1-result))
          (parser1 input state)))))))

;; Combines [parser1] and [parser2], deferring to parser1's result.
(def combine-parser-left
  (lambda parser1
    (lambda parser2
      (map-parser-value (combine-parser parser1 parser2) first))))

;; Combines [parser1] and [parser2], deferring to parser2's result.
(def combine-parser-right
  (lambda parser1
    (lambda parser2
      (map-parser-value (combine-parser parser1 parser2) second))))

;; Parses a list of [parser].
(def repeat-parser
  (lambda parser
    (lambda input
      (lambda state
        ((lambda result
          (if (is-parse-success result)
            ((lambda rest-result
              (parse-success
                (cons
                  (parse-success.value result)
                  (parse-success.value rest-result))
                (parse-success.state rest-result)
                (parse-success.rest rest-result)))
            (repeat-parser parser
              (parse-success.rest result)
              (parse-success.state result)))
            (parse-success nil state input)))
        (parser input state))))))

;; Parses a non empty list of [parser].
(def repeat-parser1
  (lambda parser
    (map-parser-value
      (combine-parser parser (repeat-parser parser))
        (lambda pair
          (cons (first pair) (second pair))))))

;; Parses [parser2] if [parser1] fails.
(def alternative-parser
  (lambda parser1
    (lambda parser2
      (lambda input
        (lambda state
          ((lambda parser1-result
            (if (is-parse-success parser1-result)
              parser1-result
              (parser2 input state)))
          (parser1 input state)))))))

;; A parser whose implementation is only evaluated once it is called.
(def lazy-parser
  (lambda parser
    (lambda input
      (lambda state
        ((parser nil) input state)))))