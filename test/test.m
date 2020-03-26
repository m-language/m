#(Asserts that a condition is true, or fails with an error message)
(defn (assert message cond cont)
  (cond cont (error message)))

#(Asserts that two values are equal, or fails with an error message)
(defn-generic equatable (assert-eq message a b cont)
  (assert message (eq a b) cont))

#(Runs a test)
(defn (run-test test)
  (test "Tests passed"))

#(Tests for M)
(def test-m
  (chain [test-fn test-def test-block test-int]))

#(Tests for fn)
(def test-fn
  (chain {
    (assert "(x -> x) true => true" (fn x x (bool true)))
    (assert "(x -> y -> x) true _ => true" (fn x (fn y x) (bool true) (bool false)))
    (assert "(x y -> x) true _ => true" (fn [x y] x (bool true) (bool false)))
    (assert "fnx = (x ->), fnx x true => true" (fn fnx (fnx x (bool true)) (fn x)))
  }))

#(Tests for def)
(def test-def
  (chain {
    (assert "x = true => x = true" (def x (bool true) x))
    (assert "defx = (x =), defx true => x = true" (def defx (def x) (defx true x)))
  }))

#(Tests for block)
(def test-block
  (chain {
    (assert "{x = true} => x = true" (block {(def x (bool true))} x))
  }))

#(Tests for integers)
(def test-int
  (import [int bool]
    (chain {
      (assert "18446744073709551616 == 18446744073709551616" (eq 18446744073709551616 18446744073709551616))
      (assert "!(-18446744073709551616 == 18446744073709551616)" (not (eq -18446744073709551616 18446744073709551616)))
      (assert "-18446744073709551616 < 18446744073709551616" (lt -18446744073709551616 18446744073709551616))
      (assert "!(0 < 0)" (not (lt 0 0)))
      (assert "18446744073709551616 > -18446744073709551616" (gt 18446744073709551616 -18446744073709551616))
      (assert "!(0 > 0)" (not (gt 0 0)))
      (assert-eq int "1 + 18446744073709551615 == 18446744073709551616" (add 1 18446744073709551615) 18446744073709551616)
      (assert-eq int "-18446744073709551615 - 1 == -18446744073709551616" (sub -18446744073709551615 1) -18446744073709551616)
      (assert-eq int "2 * 9223372036854775808 == 18446744073709551616" (mul 2 9223372036854775808) 18446744073709551616)
      (assert-eq int "18446744073709551616 / 2 == 9223372036854775808" (div 18446744073709551616 2 (error "x / 0")) 9223372036854775808)
      (assert "1 / 0 == NaN" (div 1 0 true))
    })))