# Revision history for falsify

## 0.4.0 -- ??

This release is a major cleanup release, and backwards incompatible with 0.3 in
various ways. Below we provide a list of changes as well as a migration guide.

### Package split: `falsify` vs `tasty-falsify`

* The main `falsify` package no longer provides `tasty` integration (indeed,
  it does not depend on `tasty` at all anymore); instead, `tasty` integration
  is provided by a new `tasty-falsify` package (#80).

* Module `Test.Tasty.Falsify` now lives in `tasty-falsify`, and it _only_
  provides the functionality required for the use of `falsify` with `tasty`; it
  no longer re-exports anything from `falsify`.

* `Test.Falsify.Driver` now provides the "official" API for use in driver
  integration.

### Module hierarchy

The module hierarchy has been improved.

* There is a new top-level module `Test.Falsify`, which is intended to provide
  all definitions that are meant for unqualified import. A typical `falsify`
  test suite will therefore start with

  ```hs
  import Test.Falsify
  import qualified Test.Falsify.Generator as Gen
  import qualified Test.Falsify.Predicate as P
  import qualified Test.Falsify.Range     as Range
  ```

* `Test.Falsify.Generator` now really only provides generators
  (it was previously an awkward mix of generators and custom datatypes)

* `Data.Falsify.*` is a new public module hierarchy providing some general
  purpose data structures and utilities:
  - `Data.Falsify.Concrete`
  - `Data.Falsify.Permutation`
  - `Data.Falsify.ProperFraction`
  - `Data.Falsify.Tree`
  - `Data.Falsify.WordN`

* Some testing specific data structures now have dedicated modules:
  - `Test.Falsify.Fun`
  - `Test.Falsify.Marked`
  - `Test.Falsify.SampleTree`
  - `Test.Falsify.ShrinkTree`

The internal (private) module hierarchy has been simplified as well; the
`Reexported.*` module hierarchy is no longer used (the distinction had lost its
purpose).

### Newtypes

A number of type aliases have been replaced by newtypes.

* In `Test.Falsify.Predicate`, `VarName` is now a newtype rather than a type
  alias, and we now also have `FnName` alongside `VarName`. Both of these have
  `IsString` instances, so code that uses `OverloadedStrings` should not be
  affected

* `ShrinkTree` is now a newtype rather than a type alias, and is moved to
  `Test.Falsify.ShrinkTree`

### Other changes

* The signature of `path` has been simplified: it no longer uses `IsValidShrink`
  (which is now internal API)
* Simplified signature of `bst`, which now only accepts inclusive bounds (#91)
* Remove deprecated functions `integral` and `enum`
* Rename `Test.Falsify.Range.between` to `uniform` (#92).
  This avoid confusion with `Test.Falsify.Predicate.between` (since the values
  produced by the former do not necessarily satisfy the latter!). Also improved
  documentation.
* Predicates `alwaysPass` and `alwaysFail` have been renamed to `pass` and
  `fail` respectively, and `fail` now takes an error messages. The new names
  are a more natural fit when used together with `lam` to construct predicates
  of arbitrary arity. To consider the old `alwaysFail`, use `Fail "Fail"`.
* Predicate documentation has been significantly improved.
* `Test.Falsify.Interactive` now offers a pure function `sampleUsing` (#65),
  using `ReplaySeed` to initialize the PRNG. `ReplaySeed` is no longer opaque.
* Dropped support for GHC < 8.10.7

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
