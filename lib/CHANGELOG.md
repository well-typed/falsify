# Revision history for falsify

## 0.3.0 -- 2026-03-05

* Introduce new `Range` constructor called `between`, which can be used for
  better uniform selection of large bit-size `Integral` types.
  [#81, reported by Andrea Vezzosi]
* Support generating functions from empty types [#84, Sjoerd Visscher]
* Add `minimalValue` function [#86, Sjoerd Visscher]
* Fix overflow in `Fun Int8` [#89, reported by Jake McArthur]
* The primitive `Range` constructor is now based on `WordN` rather than
  `ProperFraction`. Most users will not notice this difference, but the
  signature of the primitive `eval` function has changed.
* Relax package bounds and test with ghc 9.14.1

## 0.2.0 -- 2023-11-08

* Avoid use of `Expr` in `at` (#48)
* Add `oneof` (#54; Simon Kohlmeyer)
* Generalize `Range`, so that it can be used for types like `Char` (#51).
  As a consequence, `Gen.integral` and `Gen.enum` are now deprecated, and
  superseded by `Gen.inRange`.
* Add `GenDefault` class and `DerivingVia` helpers to derive generators
  (Eric Conlon; #61, #64).

## 0.1.1 -- 2023-04-07

* Better verbose mode for test failures
* New predicates: `split` and `pairwise`.
* Shrink towards the _second_ half of the range in `withOrigin`

## 0.1.0 -- 2023-04-05

* First release
