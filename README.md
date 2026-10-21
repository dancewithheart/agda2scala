[![CI](https://github.com/dancewithheart/agda2scala/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/dancewithheart/agda2scala/actions?query=workflow%3A%22build%22+branch%3Amain)

An **experimental Agda backend** that generates Scala 2 or Scala 3 source files from Agda (formal verification).

For Agda code you want to convert to Scala add pragma `AGDA2SCALA:
```agda
{-# COMPILE AGDA2SCALA Rgb #-}
```
## Supported features

## Supported features

* ✅ Sum types / ADTs
  * ✅ simple and polymorphic ADTs
  * ✅ constructors with arguments
  * ✅ recursive datatypes
  * ✅ inferred covariance (`RedBlackTree[+V]`)
  * ✅ polymorphic nullary constructors using `Nothing`

* ✅ Product types / records
  * ✅ simple and polymorphic records
  * ✅ operation records -> Scala traits
  * ✅ record values -> Scala trait implementations (`Monoid`)
  * ✅ higher-kinded parameters (`F[_]`)
  * ✅ polymorphic method declarations (`Functor.map`)
  * 🚧 polymorphic method implementations (`Functor` instances)

* ✅ Functions
  * ✅ simple and polymorphic functions (`id[A]`)
  * ✅ higher-kinded type parameters
  * ✅ visible arguments and erased implicit type arguments
  * ✅ constructor application and recursive calls
  * ✅ selected higher-order functions
  * 🚧 general lambda expressions and local `let` bindings
  * 🚧 mutually recursive functions

* ✅ Pattern matching
  * ✅ nullary, flat, and nested constructor patterns
  * ✅ catch-all branches and fallback propagation
  * ✅ capture-avoiding pattern-variable generation
  * ✅ correct handling of Agda case environments
  * 🚧 literal branches and projection patterns

* ✅ Literals and operators
  * ✅ `Nat` -> `Long`, `Bool`, `String`
  * ✅ conditionals and selected binary operators
  * ✅ natural-number comparisons
  * 🚧 more literals and operators

* ✅ Scala 2 and Scala 3
  * ✅ sealed traits, case classes, and case objects (Scala 2)
  * ✅ enums and enum cases (Scala 3)
  * ✅ polymorphic ADTs with variance
  * ✅ Red-Black Tree lookup, insertion, and balancing
  * ✅ end-to-end Scala tests

* 🚧 Property-based testing from Agda specifications
  * ✅ prototype: equality propositions → ZIO Test properties (#148)
  * ✅ generated tests for Scala 2 and Scala 3
  * ✅ one quantified `Nat` argument
  * 🚧 separate `AGDA2SCALA_TEST` pragma and property-test IR
  * 🚧 multiple quantified arguments and type-directed generators
  * 🚧 Monoid and Functor laws

* 🚧 Variance inference
  * ✅ positive and negative occurrences
  * ✅ invariant fallback for mixed occurrences
  * ✅ conservative handling of unknown type constructors
  * 🚧 variance propagation through known type constructors
  * 🚧 contravariance and record variance validation

* 🚧 typeclasses
  * 🚧 Agda instance arguments -> Scala 3 `given` / `using`

* 🚧 advanced types (dependent types, HKTs)
  * 🚧 partial type application -> type lambdas (`[X] =>> ...`)
  * ✅ dependent records -> path-dependent types
  * 🚧 dependent types / type-level computation -> match types
  * 🚧 general copatterns and coinduction (e.g. for Streams in Scala)
  * 🚧 existential types (?)

* Agda type guarantees -> Scala representation
  * 🚧 (?) termination and totality guarantees
  * 🚧 (?) Agda Pos => wrappend Int with smart constructor with assertions / library refined (?)


Planed milestones:
* support FP abstractions: Functor, Monad: https://github.com/dancewithheart/agda2scala/issues/12
* convert Agda proofs into property tests e.g. Monoids, Functor (create generators based on inductive definitions)
* convert some Agda definitions to:
   - Scala 3 match types
   - Scala 2 / 3 path dependent types
* convert coinductive definitions from Agda into lazy streams in Scala
* (?) support more languages: Kotlin, Java, JVM bytecode

## Usage

```sh
cabal run -- agda2scala ./examples/rbt.agda
cabal run -- agda2scala --scala-dialect=Scala2 --out-dir=scala2/src/main/scala ./examples/Monoids.agda
```

# Examples

[Agda examples](./examples/adts.agda) and generated [Scala 3 code](./scala3/src/main/scala) and [Scala 2 code](./scala2/src/main/scala).

## Example RedBlackTree

```agda
module examples.rbt where

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Nat using (ℕ; _<ᵇ_)

data Color : Set where
  Red Black : Color
{-# COMPILE AGDA2SCALA Color #-}

data RedBlackTree (V : Set) : Set where
  EmptyRBT : RedBlackTree V
  RBT      : Color -> RedBlackTree V -> ℕ -> V -> RedBlackTree V -> RedBlackTree V
{-# COMPILE AGDA2SCALA RedBlackTree #-}

lookup : {V : Set} -> V -> ℕ -> RedBlackTree V -> V
lookup defaultVal key EmptyRBT =
  defaultVal
lookup defaultVal key (RBT c left currKey x right) =
  if key <ᵇ currKey then
    lookup defaultVal key left
  else
    if currKey <ᵇ key then
      lookup defaultVal key right
    else
      x
{-# COMPILE AGDA2SCALA lookup #-}
```

generated Scala:

```scala
package examples

object rbt:
  enum Color:
    case Red
    case Black

  enum RedBlackTree[V]:
    case EmptyRBT extends RedBlackTree[Nothing]
    case RBT[V](x0: Color, x1: RedBlackTree[V], x2: Long, x3: V, x4: RedBlackTree[V]) extends RedBlackTree[V]

  def lookup[V](x1: V, x2: Long, x3: RedBlackTree[V]): V = x3 match
    case RedBlackTree.EmptyRBT =>
      x1
    case RedBlackTree.RBT(p0, p1, p2, p3, p4) =>
      if x2 < p2 then
        lookup(x1, x2, p1)
      else if p2 < x2 then
        lookup(x1, x2, p4)
      else
        p3
```

## Architecture

We use 2 internal representation
1. [AgdaIR](./src/Agda/Compiler/Scala/IR/AgdaIR.hs) is close to Agda compiler representation
2. [ScalaIR](./src/Agda/Compiler/Scala/IR/ScalaIR.hs) is close to Scala language representation

```text
             TCM glue                      prettyPrint
Definition ==========> AgdaIR ==> ScalaIR ============> String
```

At the end we choose between [Scala 2](./src/Agda/Compiler/Scala/Render/PrintScala2.hs) and [Scala 3](./src/Agda/Compiler/Scala/Render/PrintScala3.hs) syntax.

## Identifier mapping

Agda constructor names are not always valid Scala identifiers (e.g. `[]`).
We apply a small mapping table for common constructors and then sanitize the result:

- `[]` → `Nil`
- `_∷_` / `_::_` / `_cons_` → `Cons`
- `_,_` → `Pair`

See [Agda.Compiler.Scala.Name.NamePolicy](./src/Agda/Compiler/Scala/Name/NamePolicy.hs).

## Working with source code

* continuous compilation loop using [entr](https://eradman.com/entrproject/)

```shell
find -name '*.hs' | entr cabal test all
find . -name '*.hs' | entr cabal test agda2scala-test
find . -name '*.hs' | entr cabal test agda2scala-props
```

or using [ghcid](https://hackage.haskell.org/package/ghcid)
```shell
ghcid
```

* Build

```sh
cabal build all
```

* Run tests

```sh
cabal test all
cabal test agda2scala-test
cabal test agda2scala-props
```

* static code analysis using [HLint](https://hackage.haskell.org/package/hlint/docs/Language-Haskell-HLint.html):
```sh
hlint src app test
```

* format code (optionally)
```sh
fourmolu -i $(find src app test -name '*.hs')
cabal-fmt -i agda2scala.cabal
```

## end-to-end tests

There are [Scala 3](./scala3) and [Scala 2](./scala2) projects for code
generated from [Agda examples](./examples/adts.agda) they are [checked CI on Github](/.github/workflows/haskell.yml), they include unit tests
to execute code generated from Agda.
```text
                agda2scala                
                (generate)                     sbt test
Agda examples ==============> src/main/scala <============ src/test/scala
```
Locally (you need [SBT](https://www.scala-sbt.org/2.x/docs/en/Setup.html):
```sh
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala2 --out-dir=scala2/src/main/scala ./examples/adts.agda
cd scala3
sbt test
```


## Resources
* Documentation for [Agda as Haskell library on Hackage](https://hackage.haskell.org/package/Agda) including
  * docs for [Agda.Compiler.Backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-Backend.html)
  * build-in [JS backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-JS-Compiler.html)
  * build-in [Haskell backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-MAlonzo-Compiler.html)
* other backends:
  * Haskell [agda/agda2hs](https://github.com/agda/agda2hs), ([publication](https://iohk.io/en/research/library/papers/reasonable-agda-is-correct-haskell-writing-verified-haskell-using-agda2hs/))
  * Rust [omelkonian/agda2rust](https://github.com/omelkonian/agda2rust)
  * [jespercockx/agda2scheme](https://github.com/jespercockx/agda2scheme)
  * [omelkonian/agda-minimal-backend](https://github.com/omelkonian/agda-minimal-backend)
  * [omelkonian/agda2train](https://github.com/omelkonian/agda2train)
  * [HectorPeeters/agda2rust](https://github.com/HectorPeeters/agda2rust), ([publication](https://repository.tudelft.nl/islandora/object/uuid:39bff395-1bd6-4905-8554-cef0cd5e7d3e))
