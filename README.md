[![CI](https://github.com/dancewithheart/agda2scala/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/dancewithheart/agda2scala/actions?query=workflow%3A%22build%22+branch%3Amain)

An **experimental Agda backend**: generates Scala 2 or Scala 3 source files from Agda (formal verification).


## Example Red Black Tree

This ([more commands in CI script](.github/workflows/haskell.yml)):

```sh
cabal run -- agda2scala --scala-dialect=Scala3 --out-dir=scala3/src/main/scala ./examples/adts.agda
```

[Agda](examples/rbt.agda) ([translated in agda-verified-fp-algos](https://github.com/dancewithheart/agda-verified-fp-algos/blob/main/src/VerifiedAlgos/RedBlackTree.agda) from [SF vol 3](https://softwarefoundations.cis.upenn.edu/vfa-current/Redblack.html)):

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

is translated to [Scala 3](scala3/src/main/scala/examples/rbt.scala):

```scala
package examples

object rbt:
  enum Color:
    case Red
    case Black

  enum RedBlackTree[+V]:
    case EmptyRBT extends RedBlackTree[Nothing]
    case RBT(x0: Color, x1: RedBlackTree[V], x2: Long, x3: V, x4: RedBlackTree[V]) extends RedBlackTree[V]

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

## Supported features

* ✅ simple sum types and product types

[Agda](examples/adts.agda)
```agda
-- simple sum type with arguments

data Color : Set where
  Light : Rgb -> Color
  Dark : Rgb -> Color
{-# COMPILE AGDA2SCALA Color #-}

-- simple sum type

record RgbPair : Set where
  constructor mkRgbPair
  field
    fst : Rgb
    snd : Answer
{-# COMPILE AGDA2SCALA RgbPair #-}

```
[Scala 3](scala3/src/main/scala/examples/adts.scala)
```scala
enum Color:
  case Lig 3ht(x0: Rgb)
  case Dark(x0: Rgb)

final case class RgbPair(fst: Rgb, snd: Answer)
```
[Scala 2](scala3/src/main/scala/examples/adts.scala)
```scala
sealed trait Color
object Color {
  final case class Light(x0: Rgb) extends Color
  final case class Dark(x0: Rgb) extends Color
}

final case class RgbPair(fst: Rgb, snd: Answer)

```

* ✅ polymorphic ADTs and functions (pattern matching, no let yet)
  - constructors with arguments
  - simple variance handling (only covariant types), case object in Scala 2
  - recursive definitions
  - pattern matching

[Agda](examples/higher_order_fun.agda)

```agda
-- polymorphic ADTs

data List (X : Set) : Set where
  []   : List X
  _::_ : X -> List X -> List X
{-# COMPILE AGDA2SCALA List #-}

map : {A B : Set} ->  (A -> B) -> List A -> List B
map f []        = []
map f (x :: xs) = f x :: map f xs
{-# COMPILE AGDA2SCALA map #-}

foldr : {A B : Set} ->  (A -> B -> B) -> B -> List A -> B
foldr f init []        = init
foldr f init (x :: xs) = f x (foldr f init xs)
{-# COMPILE AGDA2SCALA foldr #-}

filter : {A : Set} -> (A -> Bool) -> List A -> List A
filter f []        = []
filter f (x :: xs) with f x
... | false = filter f xs
... | true  = x :: (filter f xs)
{-# COMPILE AGDA2SCALA filter #-}
```

[Scala](scala3/src/main/scala/examples/higher_order_fun.scala):
```scala
  enum List[+X]:
    case Nil extends List[Nothing]
    case Cons(x0: X, x1: List[X]) extends List[X]

  def emptyNatList(): List[Long] = List.Nil

  def map[A, B](x2: A => B, x3: List[A]): List[B] = x3 match
    case List.Nil =>
      List.Nil
    case List.Cons(p0, p1) =>
      List.Cons(x2(p0), map(x2, p1))

  def foldr[A, B](x2: A => B => B, x3: B, x4: List[A]): B = x4 match
    case List.Nil =>
      x3
    case List.Cons(p0, p1) =>
      x2(p0)(foldr(x2, x3, p1))

  def filter[A](x1: A => Boolean, x2: List[A]): List[A] = x2 match
    case List.Nil =>
      List.Nil
    case List.Cons(p0, p1) =>
      with_78(x1, p0, x1(p0), p1)

  def with_78[A](x1: A => Boolean, x2: A, x3: Boolean, x4: List[A]): List[A] = x3 match
    case false =>
      filter(x1, x4)
    case true =>
      List.Cons(x2, filter(x1, x4))

  def maybe[A, B](x2: B, x3: A => B, x4: Maybe[A]): B = x4 match
    case Maybe.Just(p0) =>
      x3(p0)
    case Maybe.None =>
      x2
```

* Agda records as scala traits
* Property tests generation (zio-test)

[Monoids](examples/Monoids.agda)
```agda
module examples.Monoids where

open import Agda.Builtin.Equality using (_≡_)
open import Data.Nat using (ℕ; _+_; zero)
open import Data.Nat.Properties using
  (+-assoc; +-identityˡ; +-identityʳ)

record Monoid (A : Set) : Set where
  field
    empty   : A
    combine : A → A → A
{-# COMPILE AGDA2SCALA Monoid #-}

combine3 : {A : Set} → Monoid A → A → A → A → A
combine3 m x y z = combine x (combine y z)
  where
    open Monoid m
{-# COMPILE AGDA2SCALA combine3 #-}

prependEmpty : {A : Set} → Monoid A → A → A
prependEmpty m x = combine empty x
  where
    open Monoid m
{-# COMPILE AGDA2SCALA prependEmpty #-}

natPlusRightUnit : (x : ℕ) → x + zero ≡ x
natPlusRightUnit x = +-identityʳ x
{-# COMPILE AGDA2SCALA natPlusRightUnit #-}

natPlusLeftUnit : (x : ℕ) → zero + x ≡ x
natPlusLeftUnit x = +-identityˡ x
{-# COMPILE AGDA2SCALA natPlusLeftUnit #-}

natPlusAssoc : (x y z : ℕ) →
  (x + y) + z ≡ x + (y + z)
natPlusAssoc x y z = +-assoc x y z
{-# COMPILE AGDA2SCALA natPlusAssoc #-} 
```

[Scala main code](scala3/src/main/scala/examples/Monoids.scala):
```scala
object Monoids:
  trait Monoid[A]:
    def empty: A
    def combine: A => A => A

  def combine3[A](x1: Monoid[A], x2: A, x3: A, x4: A): A = x1.combine(x2)(x1.combine(x3)(x4))

  def prependEmpty[A](x1: Monoid[A], x2: A): A = x1.combine(x1.empty)(x2)

  def NatPlusMonoid(): Monoid[Long] = new Monoid[Long]:
    def empty: Long = 0
    def combine: Long => Long => Long = (natArg0) => (natArg1) => natArg0 + natArg1
```

[Scala property tests](scala3/src/test/scala/examples/MonoidsPropertySpec.scala)

```scala
import zio.test._
import Monoids._

object MonoidsPropertySpec extends ZIOSpecDefault {
  def spec = suite("Monoids")(
    test("natPlusRightUnit") {
      check(Gen.long(0L, 1000000L)) { x0 =>
        assertTrue((x0 + 0) == (x0))
      }
    },
    test("natPlusLeftUnit") {
      check(Gen.long(0L, 1000000L)) { x0 =>
        assertTrue((0 + x0) == (x0))
      }
    },
    test("natPlusAssoc") {
      check(Gen.long(0L, 1000000L), Gen.long(0L, 1000000L), Gen.long(0L, 1000000L)) { (x0, x1, x2) =>
        assertTrue(((x0 + x1) + x2) == (x0 + (x1 + x2)))
      }
    }
  )
}
```

* 🚧 support more property tests libraries (pragma?)

* Higher kinded traits - Functors
  - higher-kinded parameters (`F[_]`)

* Other examples
Trivial bool solver
Yoneda
* TODO cryptography/ZK => Merkle Tree
* TODO category theory => Yoneda, Profunctor Optics
* TODO quantum circuits =>

## Roadmap

* 🚧 review & cleanup - `AGDA2SCALA_TEST` pragma
* 🚧 functor instances
* 🚧 functor laws
* 🚧 covariant types (Profunctor ?)
* 🚧 mutually recursive definitions
* 🚧 more coverage of function defintions - `let` bindings
* 🚧 local definitions (e.g. currently RGB functions are top level)
* 🚧 more dependent types using
 - 🚧 Scala 3 match types
 - 🚧 path dependent types in functions
* 🚧 more robust property tests
* 🚧 Scala type classes (Scala 3 `given` / `using`)
* 🚧 Scala type lambdas
* 🚧 general copatterns and coinduction (e.g. for Streams in Scala)
* 🚧 (?) support more languages: Kotlin, Java, JVM bytecode


## Design

## Support multiple languages

Separate [Agda IR](src/Agda/Compiler/Scala/IR/AgdaIR.hs) and [Scala IR](src/Agda/Compiler/Scala/IR/ScalaIR.hs).

Agda -> Agda IR: [toAgdaExpr](src/Agda/Compiler/Scala/Lower/AgdaToIR.hs)  
Agda IR -> Scala IR: [toScalaExpr](src/Agda/Compiler/Scala/Lower/IRToScala.hs)

🚧 Potentially we can suport multiple languages - Kotlin? Java? JVM bytecode?

🚧 Why not Python?

## Scala variance inference

[inferDataTyParams](src/Agda/Compiler/Scala/Lower/IRToScala.hs):
```Haskell
    Absent       -> Covariant
    PositiveOnly -> Covariant
    NegativeOnly -> Invariant
    Mixed        -> Invariant
```

[printVariantTyParams](src/Agda/Compiler/Scala/Render/Common.hs)

🚧 Detect contravariant types.  
🚧 Propagate variance in complex cases.  
🚧 Handle variance for polymorphic and higher kinded types.

## Dependent types

Do not choose how to handle dependent types. Configure using pragma:

```
AGDA2SCALA XXX = path
AGDA2SCALA XXX =
```
[Agda example](examples/DependentPackage.agda):
```agda
open import Data.Nat using (ℕ)

record Package : Set₁ where
  field
    Carrier : Set
    item    : Carrier

{-# COMPILE AGDA2SCALA Package = path #-}
```

Scala constructions that could support subset of Agda dependent types
```scala
// path-dependent types:

trait Package:
  type A
  def value: A

// dependent method types:
def get(p: Package): p.A = p.value

// Scala 3 match types:
type Element[X] = X match
  case List[t] => t1
  case Option[t] => t2
```

Implementation: [parseRecordStrategy]()

## Choose theorems for property tests

TODO PP

## Identifier mapping

Agda identifiers might not be valid Scala identifiers (e.g. `[]`) - so we do mapping ([Agda.Compiler.Scala.Name.NamePolicy](./src/Agda/Compiler/Scala/Name/NamePolicy.hs)):
- `[]` → `Nil`
- `_∷_` / `_::_` / `_cons_` → `Cons`
- `_,_` → `Pair`

# Correctness = (Tests => Docs) + Properties

## unit tests output => up to date + easy to understand documentation

```sh
entr cabal test agda2scala-test
```
```text
  Name.NameEnv / Scala identifier allocation
    emptyNameEnv starts with no QName mappings, no taken names, and counter 0:          OK
    sanitizeScalaIdent maps an empty name to x:                                         OK
    sanitizeScalaIdent replaces illegal Scala identifier characters with underscores:   OK
    sanitizeScalaIdent prefixes names that start with a digit:                          OK
    sanitizeScalaIdent avoids Scala keywords by adding a suffix:                        OK
    allocFreshLocal uses the sanitized base name when it is free:                       OK
    allocFreshLocal appends numeric suffixes on collisions:                             OK
    allocFreshLocal freshens names after keyword sanitization:                          OK
    numbered fresh names skip candidates already reserved:                              OK
  Name.NamePolicy / Agda constructor names
    constructor [] is rendered as Nil:                                                  OK
    constructor _::_ is rendered as Cons:                                               OK
    constructor _∷_ is rendered as Cons:                                                OK
    constructor _,_ is rendered as Pair:                                                OK
    constructor _cons_ is not rewritten by the default policy:                          OK
  Render.Common
    combineLines removes empty lines and joins non-empty lines:                         OK
    printPat parenthesizes a cons pattern in cons-head position:                        OK
    printType prints function result:                                                   OK
    printType prints function input:                                                    OK
  Render.Scala2
    module headers
      prints object when module has one name part:                                      OK
      prints package and object for two-part module name:                               OK
      prints dotted package and object for multi-part module name:                      OK
    declarations
      prints sealed trait for an ADT parent:                                            OK
      prints case object for a zero-argument constructor:                               OK
      prints case class for a record/product type:                                      OK
      prints polymorphic case class:                                                    OK
      prints monomorphic sum type:                                                      OK
      prints polymorphic sum type:                                                      OK
      prints polymorphic function definition:                                           OK
      renders a polymorphic trait with abstract members:                                OK
    layout
      prints package with handled declarations and skips empty unhandled declarations:  OK
      combineDecls strips trailing newlines before joining declarations:                OK
    pattern matching
      prints flat constructor match:                                                    OK
    it then else
      prints if then else:                                                              OK
      printer breaks if/else branches:                                                  OK
    renders abstract type members:                                                      OK
  Render.Scala3
    module headers
      prints object when module has one name part:                                      OK
    declarations
      prints sealed trait for an ADT parent:                                            OK
      prints case object for a zero-argument constructor:                               OK
      prints case class for a record/product type:                                      OK
      renders a polymorphic trait with abstract members:                                OK
    layout
      prints package with two enum declarations and skips empty unhandled declarations: OK
    pattern matching
      prints flat constructor match:                                                    OK
      prints flat constructor match with application on the right-hand side:            OK
    it then else
      prints if then else:                                                              OK
    renders abstract type members:                                                      OK
    test_concreteTypeMember:                                                            OK
    preserves binary operator grouping:                                                 OK
  Generated property tests
    compares translated expressions:                                                    OK
    generates three-argument associativity property:                                    OK
  Scala backend / options and module output
    backend is enabled by default:                                                      OK
    --out-dir stores target output directory:                                           OK
    --scala-dialect stores selected dialect:                                            OK
    initModuleEnv starts with empty NameEnv:                                            OK
    selectPrinter defaults to Scala3:                                                   OK
    selectPrinter chooses Scala3 when requested:                                        OK
    shouldWriteModule skips modules with only empty SeUnhandled definitions:            OK
    shouldWriteModule writes modules with at least one handled definition:              OK
    record constructor patterns use the generated record type:                          OK
    substituteType replaces nested type variables:                                      OK
    record construction instantiates field types:                                       OK
    record construction rejects incorrect type arity:                                   OK
    record construction rejects incorrect field arity:                                  OK
    record construction rejects mismatched result type:                                 OK
    polymorphic method implementations:                                                 OK
  Compile.Types
    type-like binder heuristic
      Sort is treated as a type-level binder:                                           OK
      Pi ending in Sort is treated as a higher-kinded type-level binder:                OK
    type variable environment
      lookupTyVar resolves the most recent type binder:                                 OK
      lookupTyVar falls back to t<i> for missing binders:                               OK
      lookupTyVar falls back to t<i> for term binders:                                  OK
    type term lowering
      compileTypeTermWith lowers a type variable to STyVar:                             OK
      compileTypeTermWith lowers applied type variable to STyApp:                       OK
      compileTypeTermWith rejects unsupported lambda terms explicitly:                  OK
      compileTypeTermWith lowers a non-dependent Pi to STyFun:                          OK
      compileTypeTermWith lowers a curried Pi to nested STyFun:                         OK
      compileTypeTermWith rejects a dependent Pi:                                       OK
    extracts type constructor arity:                                                    OK
    function schemes preserve higher-kinded parameter arity:                            OK
  Lower.Variance
    RBT is covariant:                                                                   OK
    Function input prevents covariance:                                                 OK
    Positive and negative is invariant:                                                 OK
    Unknown wrappers remain conservative:                                               OK
    higher-kinded parameters are invariant:                                             OK
```

## property tests

```sh
entr cabal test agda2scala-props
```


```text
Test suite agda2scala-props: RUNNING...
━━━ Name.NameEnv / Scala identifier hygiene ━━━
  ✓ sanitized identifiers are never empty passed 100 tests.
  ✓ sanitized identifiers contain only Scala identifier characters passed 100 tests.
  ✓ sanitized identifiers are never Scala keywords passed 100 tests.
  ✓ sanitizing an already sanitized identifier is stable (idempotent) passed 100 tests.
  ✓ fresh local allocation returns unique names passed 100 tests.
  ✓ fresh local allocation is deterministic for the same inputs passed 100 tests.
  ✓ fresh local allocation never returns a Scala keyword passed 100 tests.
  ✓ fresh local allocation marks every returned name as taken passed 100 tests.
  ✓ first allocation returns the sanitized base name when it is available passed 100 tests.
  ✓ numbered fresh names have the requested count and avoid reserved names passed 100 tests.
  ✓ numbered fresh names are never reused across successive allocations passed 100 tests.
  ✓ record constructor registration round-trips passed 100 tests.
  ✓ new record constructor registration replaces the old owner passed 100 tests.
  ✓ registered nullary terms are recognized passed 100 tests.
  ✓ 14 succeeded.
━━━ Render.PrintProps ━━━
  ✓ prop_scala2_stringEscapes_newline passed 100 tests.
  ✓ prop_scala2_stringEscapes_quote passed 100 tests.
  ✓ prop_scala2_boolLowercase passed 100 tests.
  ✓ prop_scala3_stringEscapes_newline passed 100 tests.
  ✓ prop_escape_no_raw_newlines passed 100 tests.
  ✓ prop_printScala3_type_total passed 100 tests.
  ✓ prop_scala2_prints_type_params_iff_present passed 100 tests.
  ✓ higher-kinded record parameters render in both dialects passed 100 tests.
  ✓ higher-kinded function parameters render in both dialects passed 100 tests.
  ✓ higher-kinded datatypes render in both dialects passed 100 tests.
  ✓ 10 succeeded.
━━━ CompileProps ━━━
  ✓ prop_lookupVar_inRange passed 100 tests.
  ✓ prop_lookupVar_outOfRange passed 100 tests.
  ✓ prop_compileTypeTerm_var_isTyVar passed 100 tests.
  ✓ prop_compileBodyTerm_lits passed 100 tests.
  ✓ prop_compileBodyTerm_varResolves passed 100 tests.
  ✓ prop_compileTypeTerm_total_onSubset passed 100 tests.
  ✓ prop_typeVar_roundtrip passed 100 tests.
  ✓ prop_compileBodyTerm_varLaw passed 100 tests.
  ✓ prop_compileBodyTerm_literals passed 100 tests.
  ✓ prop_compileTypeTermWith_var_app_arity passed 100 tests.
  ✓ 10 succeeded.
━━━ IRToScala ━━━
  ✓ data lowering preserves constructor arity passed 100 tests.
  ✓ data lowering applies constructor NamePolicy passed 100 tests.
  ✓ record lowering preserves field count passed 100 tests.
  ✓ operation records lower to Scala traits passed 100 tests.
  ✓ abstract type members produce a trait passed 100 tests.
  ✓ 5 succeeded.
━━━ Terms / Env / application ━━━
  ✓ Def with k Apply args compiles to application of arity k passed 100 tests.
  ✓ pattern vars extend Env with newest binder at index 0 passed 100 tests.
  ✓ function args enter Env with last arg at index 0 passed 100 tests.
  ✓ lookupVar reports index and Env size when out of range passed 100 tests.
  ✓ hidden application arguments do not become runtime Scala arguments passed 100 tests.
  ✓ function type params stay in Env as erased de Bruijn slots passed 100 tests.
  ✓ case arguments use source-order positions including erased binders passed 100 tests.
  ✓ case branch environment removes the scrutinized argument passed 100 tests.
  ✓ constructor branch binders are added after dropping the case scrutinee passed 100 tests.
  ✓ catch-all branches retain the scrutinized runtime argument passed 100 tests.
  ✓ constructor fields replace the scrutinized argument at its source position passed 100 tests.
  ✓ constructor fields replace the scrutinized slot in source order passed 100 tests.
  ✓ replaceCaseArg makes fields addressable in source order passed 100 tests.
  ✓ case splits with no runtime alternatives are rejected passed 100 tests.
  ✓ function-valued Var applications are curried passed 100 tests.
  ✓ 15 succeeded.
━━━ Compile.Types properties ━━━
  ✓ prop_lookupTyVar_resolves_named passed 100 tests.
  ✓ prop_compileTypeTermWith_varApp_arity passed 100 tests.
  ✓ prop_compileTypeTermWith_shifted_index passed 100 tests.
  ✓ 3 succeeded.
━━━ Terms / Env / application ━━━
  ✓ occurrence evidence forms a monoid passed 100 tests.
  ✓ 1 succeeded.
━━━ NamePolicy / naming ━━━
  ✓ Agda ctor -> valid Scala identifier passed 100 tests.
  ✓ is idempotent passed 100 tests.
  ✓ [] ∷  -> Nil Cons etc passed 100 tests.
  ✓ scalaKeywords -> valid Scala identifier passed 100 tests.
  ✓ 4 succeeded.
━━━ NamePolicy / merge ━━━
  ✓ is right biased passed 100 tests.
  ✓ Semigroup: associative passed 100 tests.
  ✓ Monoid: left identity passed 100 tests.
  ✓ Monoid: right identity passed 100 tests.
  ✓ 4 succeeded. 
```

TODO PP

* End to end (E2E) tests on CI

There are [Scala 3](./scala3) and [Scala 2](./scala2) projects using [SBT](https://www.scala-sbt.org/) (Scala package manager).  
[SBT config for Scala3 project](scala3/build.sbt).  
Code generated from [Agda examples](./examples/adts.agda) (validated on [CI on Github](/.github/workflows/haskell.yml)).
There are Scala tests to check generated Scala code executes.

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

## Dev notes

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
