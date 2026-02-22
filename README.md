[![CI](https://github.com/dancewithheart/agda2scala/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/dancewithheart/agda2scala/actions?query=workflow%3A%22build%22+branch%3Amain)

# agda2scala

WIP Agda to Scala 2 and Scala 3 backend

Basic usage:
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

data Color : Set where
  Light : Rgb -> Color
  Dark : Rgb -> Color
{-# COMPILE AGDA2SCALA Color #-}

-- product types

record RgbPair : Set where
  constructor mkRgbPair
  field
    fst : Rgb
    snd : Bool
{-# COMPILE AGDA2SCALA RgbPair #-}

-- simple functions: const, id

const : (rgbPairArg : RgbPair) -> (rgbArg : Rgb) -> RgbPair
const rgbPairArg rgbArg = rgbPairArg
{-# COMPILE AGDA2SCALA const #-}

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

  enum Color:
    case Light(x0: Rgb)
    case Dark(x0: Rgb)

  final case class RgbPair(fst: Rgb, snd: Bool)

  def const(rgbPairArg: RgbPair, rgbArg: Rgb): RgbPair = rgbPairArg

  def hello(): String = "Hello World!"
```

More [Agda examples](./examples/adts.agda)

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
  * [lemastero/agda2rust](https://github.com/lemastero/agda2rust)
