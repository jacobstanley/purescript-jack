[![Latest Release](http://img.shields.io/bower/v/purescript-jack.svg)](https://github.com/jystic/purescript-jack/releases)
[![Build Status](https://travis-ci.org/jystic/purescript-jack.svg?branch=master)](https://travis-ci.org/jystic/purescript-jack)
[![Dependency Status](https://www.versioneye.com/user/projects/57f641d59907da004fa9a7e8/badge.svg?style=flat)](https://www.versioneye.com/user/projects/57f641d59907da004fa9a7e8)

# purescript-jack

```
Jack's love of dice has brought him here, where he has taken on the form
of a PureScript library, in order to help you gamble with your propositions.
```

![](img/dice.jpg)

Jack is a testing library in the spirit of Hughes & Classen's
[QuickCheck](https://web.archive.org/web/20160319204559/http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf).

What makes it different is that instead of generating a random value and
using a shrinking function after the fact, we generate the random value
and all the possible shrinks in a rose tree, all at once.

## Why Jack?

Generating the shrinks when you generate your initial value has many
advantages.

It is easy to maintain the invariants of a generator for example. With
QuickCheck style shrinking if you do `chooseInt 100 200` and then try to
shrink it, it will happily shrink to `0`.

QuickCheck shrinking functions are also invariant, as the `a` appears on
both sides of the arrow in the shrink function `a -> List a`.

So if you imagine the following scenario:

```purescript
newtype Foo = Foo String

shrinkString :: String -> List String
shrinkString = ...
```

`shrinkString` cannot be lifted to work over `Foo` without having
a mapping in both directions. This breaks the beautiful applicative
syntax that generators can be constructed with. Jack doesn't have this
problem, a `Gen String` can be turned in to a `Gen Foo` using only
`map`, but with the benefit that your `Foo` will be shrunk for free.

## Getting Started

The easiest way to get started with Jack is to install it as a dev dependency:

```
$ bower install --save-dev purescript-jack
```

Then create a `test/Main.purs` as shown below:

```purescript
module Test.Main (
    main
  ) where

import Effect (Effect)

import Jack.Runner (jackMain)

import Prelude

main :: Effect Unit
main =
  jackMain [
    -- List all the modules which contain property tests here, for this
    -- example we have just one module: Test.DiceGames
      "Test.DiceGames"
    ]
```

Jack has a test runner, invoked via `jackMain`, which scans the given
modules for properties. Any exported function of type `Property` with
the prefix `prop_` will be found and exercised.

Lets take a look at `test/DiceGames.purs` for a contrived example:

```purescript
module Test.DiceGames where

import Data.Array as Array
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)

import Jack (Property, Gen, property, forAll, boundedInt)

import Prelude

genEven :: Gen Int
genEven =
  map (\x -> (x / 2) * 2) boundedInt

genEvenString :: Gen String
genEvenString =
  map show genEven

evens :: Array Char
evens =
  toCharArray "02468"

prop_even_strings_end_with_evens :: Property
prop_even_strings_end_with_evens =
  forAll genEvenString \str ->
    case Array.last $ toCharArray str of
      Nothing ->
        property false -- numbers should always have at least one digit
      Just x ->
        property $ elem x evens
```

Here we're verifying that even numbers always end with an even digit. We
do this by generating random even numbers as strings and checking that
the last character is what we expect.

If we run this example using `pulp test` we get the following output:

```
$ pulp test
* Building project in /home/jack/example
* Build successful.
* Running tests...
=== prop_even_strings_end_with_evens from Test.DiceGames ===
+++ OK, passed 100 tests.

* Tests OK.
```

Let's sabotage the test so that we're generating odd numbers instead of
evens:

```purescript
genEven :: Gen Int
genEven =
  map (\x -> (x / 2) * 2 + 1) boundedInt -- note the + 1
```

And run our example again with `pulp test`:

```
$ pulp test
* Building project in /home/jack/example
* Build successful.
=== prop_even_strings_end_with_evens from Test.Foo ===
*** Failed! Falsifiable (after 1 test and 1 shrink):
"1"

* ERROR: Subcommand terminated with exit code 1
```

You can see we get a failure, and after shrinking we're presented with
the minimal possible counterexample, `"1"`.

Note that we didn't have to do any extra work to get shrinking of our
even or odd numbers. Even shrinking a string which must be a constrained
number happens automatically. With traditional QuickCheck shrinking we
would have had to parse the string, shrink the number, check that's it's
still even (or odd), and then convert it back to a string again.

## Limitations

Many of the features you'd expect from a QuickCheck library are still
missing, but we'll get there eventually:

- Monadic / effectful tests
- Generating functions
- Model testing

Jack doesn't have an `Arbitrary` type class, by design. `Arbitrary`
instances often end up being orphans and I consider this problematic,
especially in PureScript. The main purpose of the `Arbitrary` class as
I see it is to link the generator with a shrink function, this isn't
required with Jack so `Arbitrary` has been eliminated.

This library is still very new, and I wouldn't be surprised if some of
the combinators are still a bit buggy. Jack relies heavily on laziness
and it wasn't something that I had to worry about when I wrote it for
Haskell, so I'm fixing stack overflows and out of memory errors as they
appear.

## Developing

Ensure PureScript is installed and available on your path. [Getting
started instructions](http://www.purescript.org/learn/getting-started/)
are available on the PureScript site.

Then run `bower` to install the PureScript packages:

     bower install

Finally, run `pulp` to build `purescript-jack`:

     pulp build
     pulp test
