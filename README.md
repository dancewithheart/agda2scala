[![CI](https://github.com/dancewithheart/agda2scala/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/dancewithheart/agda2scala/actions?query=workflow%3A%22build%22+branch%3Amain)

An **experimental Agda backend** that generates Scala 2 or Scala 3 source files.

Currently supported:

* ✅ Sum types / ADTs
  * ✅ simple ADTs
  * ✅ polymorphic ADTs, e.g. `Maybe[A]`, `List[A]`
  * ✅ constructors with arguments
  * ✅ inferred covariance for generated ADTs where safe, e.g. `RedBlackTree[+V]`
  * ✅ nullary polymorphic constructors lowered with `Nothing`, e.g. `EmptyRBT extends RedBlackTree[Nothing]`
* ✅ Product types / records
  * ✅ simple records
  * ✅ records with polymorphic parameters
* ✅ Simple and polymorphic functions, e.g. `id[A]`
* ✅ Literals
  * ✅ `Int` / `Nat`
  * ✅ `Bool`
  * ✅ `String`
  * 🚧 broader literal coverage
* ✅ Function application
  * ✅ normal visible arguments
  * ✅ erased hidden/type-level Agda arguments
  * ✅ constructor application
  * ✅ recursive function calls
* ✅ Pattern matching on constructors
  * ✅ zero-arity constructors
  * ✅ flat constructor patterns with arguments
  * ✅ constructor branch binders
  * ✅ nested constructor patterns
  * ✅ catch-all branches
  * ✅ inherited catch-all propagation through nested matches
  * ✅ fresh pattern-variable generation without lexical shadowing
  * ✅ correct replacement of scrutinized arguments in Agda case environments
  * ✅ rejection of case splits with no runtime alternatives
  * 🚧 literal branches
  * 🚧 projection-pattern cases
  * 🚧 exhaustive-match simplification and removal of unreachable wildcard branches
* ✅ Basic conditionals and operators
  * ✅ Agda `if_then_else_` lowered to Scala `if ... then ... else ...`
  * ✅ selected binary operators
  * ✅ natural-number boolean comparison `_ <ᵇ _` lowered to Scala `<`
  * 🚧 broader operator coverage
* ✅ Generated Scala 2 and Scala 3
  * ✅ Scala 2 sealed traits, case classes, and case objects
  * ✅ Scala 3 enums and enum cases
  * ✅ nested pattern matches in both dialects
  * ✅ polymorphic ADTs with variance in both dialects
  * ✅ generated Red-Black Tree `lookup`
  * ✅ generated Red-Black Tree `insert` and balancing
  * ✅ runtime tests for lookup after single and multiple inserts
* ✅ 🚧 Mapping selected Agda stdlib names to Scala stdlib-like output
  * ✅ `Nat`
  * ✅ `List`
  * ✅ pairs / products
  * 🚧 broader stdlib coverage
  * 🚧 principled mapping table with types, constructors, functions, and operators
* 🚧 Variance inference
  * ✅ positive occurrences
  * ✅ negative occurrences through function inputs
  * ✅ mixed occurrences lowered invariantly
  * ✅ conservative treatment of unknown type constructors
  * ✅ algebraic occurrence accumulation using a `Monoid`
  * 🚧 variance propagation through known Scala and Agda type constructors
  * 🚧 contravariant parameters
  * 🚧 variance validation for records and other generated declarations
* 🚧 Compiler robustness
  * ✅ explicit errors for unsupported case-tree shapes
  * ✅ property tests for de Bruijn environments and source-order case indices
  * ✅ property tests for constructor-field replacement
  * ✅ property tests for fresh-name supply
  * ✅ algebraic-law tests for variance occurrence accumulation
  * 🚧 structured diagnostics with source definition names and context
  * 🚧 golden tests for generated Scala files
  * 🚧 compile generated Scala automatically as part of Haskell integration tests
* 🚧 Larger language features
  * 🚧 local `let` bindings
  * 🚧 lambda expressions
  * 🚧 higher-order functions
  * 🚧 mutually recursive functions
  * 🚧 recursive and mutually recursive datatypes
  * 🚧 dependent pairs and indexed datatypes
  * 🚧 type classes / Agda instance arguments
  * 🚧 modules, imports, and namespace mapping
  * 🚧 termination and totality assumptions in generated Scala


Roadmap is captured in milestones:
* support polymorphic ADTs: List, Red Black Tree, ZIO: https://github.com/dancewithheart/agda2scala/issues/17
* support FP abstractions: Functor, Monad: https://github.com/dancewithheart/agda2scala/issues/12

## Basic usage:
```sh
cabal run -- agda2scala ./examples/adts.agda
```

compile following Agda code:

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

into Scala code:

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

More [Agda examples](./examples/adts.agda) and [output Scala 3 code](./scala3/src/main/scala/examples/adts.scala).

## Architecture

We use 2 internal representation
1. [AgdaIR](./src/Agda/Compiler/Scala/IR/AgdaIR.hs) is close to Agda compiler representation
2. [ScalaIR](./src/Agda/Compiler/Scala/IR/ScalaIR.hs) is close to Scala language representation

```text
             TCM glue                      prettyPrint
Definition ==========> AgdaIR ==> ScalaIR ============> String
```

At the end we choose between [Scala 2](./src/Agda/Compiler/Scala/Render/PrintScala2.hs) and [Scala 3](./src/Agda/Compiler/Scala/Render/PrintScala3.hs) syntax.
Possible future directions: Kotlin, Java, JVM bytecode

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

* Simple way to run Scala backend

```sh
cabal run -- agda2scala --help
cabal run -- agda2scala ./examples/adts.agda
```

* Generate Scala2 output

```sh
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala2 --out-dir=scala2/src/main/scala ./examples/adts.agda
```

* Generate Scala3 (dotty) output

```sh
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala3 --out-dir=scala3/src/main/scala ./examples/adts.agda
```

```sh
cabal run -- agda2scala --help
cabal run -- agda2scala ./examples/adts.agda
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala2 --out-dir=scala2/src/main/scala ./examples/adts.agda
```

* format code
```sh
fourmolu -i $(find src app test -name '*.hs')
cabal-fmt -i agda2scala.cabal
```

* static code analysis using [HLint](https://hackage.haskell.org/package/hlint/docs/Language-Haskell-HLint.html):
```sh
hlint src app test
```

## end-to-end tests

There are [Scala 3](./scala3) and [Scala 2](./scala2) projects for code
generated from [Agda examples](./examples/adts.agda)

They have unit tests, that use code generated from examples.

```text
                agda2scala                
                (generate)                     sbt test
Agda examples ==============> src/main/scala <============ src/test/scala

checks:
* compile Agda examples to Scala code
* run Scala unit tests that calls that Scala code
```

Those tests are [run on CI - Github Actions](/.github/workflows/haskell.yml).

Generate Scala 2 code from Agda examples and running tests:
```shell
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala2 --out-dir=scala2/src/main/scala ./examples/adts.agda
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala2 --out-dir=scala2/src/main/scala ./examples/rbt.agda
cd scala2
sbt ~test
```

generate Scala 3 code:
```shell
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala3 --out-dir=scala3/src/main/scala ./examples/adts.agda
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala3 --out-dir=scala3/src/main/scala ./examples/rbt.agda
cd scala3
sbt ~test
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

