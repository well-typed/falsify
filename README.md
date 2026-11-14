# `falsify`—internal shrinking reimagined for Haskell

This is the home of `falsify`, a Haskell library for property-based testing with
"internal" shrinking, inspired by the Python
[Hypothesis](https://hypothesis.works/) library.

Quick TL;DR in four steps:

1. In [QuickCheck](https://hackage.haskell.org/package/QuickCheck) shrinkers
   must be manually defined, separate from generators.
2. In [Hedgehog](https://hackage.haskell.org/package/hedgehog) every generator
   implies a shrinker, but shrinking does not work across monadic bind; every
   bind introduces a "cut point". Once we start shrinking values after a monadic
   bind, values generated before the bind are now fixed.
3. The main insight of the Hypothesis library is a way to have these "implied"
   shrinkers, but without the problems with bind.
4. In `falsify` we have reimagined this in the context of Haskell, using a more
   axiomatic foundation based on a single principle, which moreover works with
   Haskell's lazy nature and infinite data structures (including random function
   generation).

References:

* The main reference is the paper "`falsify`: Internal Shrinking Reimagined for Haskell",
  [Haskell Symposium 2023](https://dl.acm.org/doi/10.1145/3609026.3609733)
  ([PDF](https://well-typed.com/blog/aux/files/falsify.pdf),
  [presentation](https://www.youtube.com/watch?v=csKkTas6X58))
* Blogpost that introduced the library
  [`falsify`: Hypothesis-inspired shrinking for Haskell](https://well-typed.com/blog/2023/04/falsify/)
* Blogpost on some new features in `falsify-0.4`:
  [New `falsify` release](https://well-typed.com/blog/2026/07/falsify-4/)

> [!WARNING]
> This library is experimental and not actively maintained. Use of the library
> as well as feedback is welcome, but Well-Typed cannot guarantee any level of
> unpaid maintenance or support. Please contact us via email to
> info@well-typed.com if you are interested in commercial support or funding
> development of the library.
