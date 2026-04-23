[![CI](https://github.com/dancewithheart/agda2scala/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/dancewithheart/agda2scala/actions?query=workflow%3A%22build%22+branch%3Amain)

An **experimental Agda backend** that generates Scala 2 or Scala 3 source files.

Currently supported:

- ✅ Sum types / ADTs (also **polymorphic ADTs** like `Maybe[A]`, `List[A]`)
- ✅ Product types (records) (with polymorphic parameters)
- ✅ Simple and polymorphic functions (e.g. `id[A]`)
- ✅ Literals: Int/Nat, Bool, String (subset)
- 🚧 Pattern matching (`match` / `cases`) – planned
- 🚧 Mapping Agda stdlib names to Scala stdlib (`Nat`, `List`, pairs) – planned / partial

## Basic usage:
```sh
cabal run -- agda2scala ./examples/adts.agda
```

compile following Agda code:

```agda
module examples.adts where

-- sum types
data Rgb : Set where
  Red : Rgb
  Green : Rgb
  Blue : Rgb
{-# COMPILE AGDA2SCALA Rgb #-}

-- polymorphic ADT

data List (X : Set) : Set where
  []   : List X
  _::_ : X -> List X -> List X
{-# COMPILE AGDA2SCALA List #-}

-- literals String

hello : String
hello = "Hello World!"
{-# COMPILE AGDA2SCALA hello #-}
```

into Scala code:

```scala
package examples

object adts:
  enum Rgb:
    case Red
    case Green
    case Blue

  enum List[X]:
    case Nil extends List[Nothing]
    case Cons[X](x0: X, x1: List[X]) extends List[X]

  def hello(): String = "Hello World!"
```

More [Agda examples](./examples/adts.agda)

## Identifier mapping

Agda constructor names are not always valid Scala identifiers (e.g. `[]`).
We apply a small mapping table for common constructors and then sanitize the result:

- `[]` → `Nil`
- `_∷_` / `_::_` / `_cons_` → `Cons`
- `_,_` → `Pair`

See `Agda.Compiler.Scala.NamePolicy`.

## Working with source code

* continuous compilation loop using [entr](https://eradman.com/entrproject/)

```shell
find -name '*.hs' | entr cabal test all
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
cabal test agda2scala-hedgehog
```

* Simple way to run Scala backend

```sh
cabal run -- agda2scala --help
cabal run -- agda2scala ./examples/adts.agda
```

* Generate Scala2 output

```sh
cabal run -- agda2scala --compile --no-main --out-dir=scala2/src/main/scala ./examples/adts.agda
```

* Generate Scala3 (dotty) output

```sh
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala3 --out-dir=scala3/src/main/scala ./examples/adts.agda
```

```sh
cabal run -- agda2scala --help
cabal run -- agda2scala ./examples/adts.agda
cabal run -- agda2scala --compile --no-main --out-dir=scala2/src/main/scala ./examples/adts.agda
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
* compile Agda examples
* run Scala unit tests that calls Scala code
```

Those tests are run on CI - Github Actions

Generate Scala 2 code from Agda examples and running tests:
```shell
cabal run -- agda2scala --compile --no-main --out-dir=scala2/src/main/scala ./examples/adts.agda
cd ../scala2
sbt ~test
```

* format code
```sh
fourmolu -i $(find src app test -name '*.hs')
cabal-fmt -i agda2scala.cabal
hlint src app test
```

generate Scala 3 code:
```shell
cabal run -- agda2scala --compile --no-main --scala-dialect=Scala3 --out-dir=scala3/src/main/scala ./examples/adts.agda
cd ../scala3
sbt ~test
```

## Resources
* Documentation for [Agda as Haskell library on Hackage](https://hackage.haskell.org/package/Agda) including
  * docs for [Agda.Compiler.Backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-Backend.html)
  * build-in [JS backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-JS-Compiler.html)
  * build-in [Haskell backend](https://hackage.haskell.org/package/Agda/docs/Agda-Compiler-MAlonzo-Compiler.html)
* external project with Agda backends
  * [omelkonian/agda-minimal-backend](https://github.com/omelkonian/agda-minimal-backend)
  * [jespercockx/agda2scheme](https://github.com/jespercockx/agda2scheme)
  * [omelkonian/agda2train](https://github.com/omelkonian/agda2train)
  * [agda/agda2hs](https://github.com/agda/agda2hs), ([publication](https://iohk.io/en/research/library/papers/reasonable-agda-is-correct-haskell-writing-verified-haskell-using-agda2hs/))
  * [HectorPeeters/agda2rust](https://github.com/HectorPeeters/agda2rust), ([publication](https://repository.tudelft.nl/islandora/object/uuid:39bff395-1bd6-4905-8554-cef0cd5e7d3e))
  * [omelkonian/agda2rust](https://github.com/omelkonian/agda2rust)
