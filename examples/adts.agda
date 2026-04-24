module examples.adts where

open import Agda.Builtin.String
open import Agda.Builtin.Nat
-- open import Data.Nat using (ℕ)

-- simple sum type no arguments - sealed trait + case objects
data Rgb : Set where
  Red : Rgb
  Green : Rgb
  Blue : Rgb
{-# COMPILE AGDA2SCALA Rgb #-}

data Answer : Set where
   Yes : Answer
   No : Answer
{-# COMPILE AGDA2SCALA Answer #-}

-- simple sum type with arguments - sealed trait + case class

data Color : Set where
  Light : Rgb -> Color
  Dark : Rgb -> Color
{-# COMPILE AGDA2SCALA Color #-}

-- simple sum type - case class

record RgbPair : Set where
  constructor mkRgbPair
  field
    fst : Rgb
    snd : Answer
{-# COMPILE AGDA2SCALA RgbPair #-}

-- trivial function with single argument

idRgb : Rgb -> Rgb
idRgb theArg = theArg
{-# COMPILE AGDA2SCALA idRgb #-}

-- const function with one named argument that returns Answer literal

rgbConstYes1 : (rgb : Rgb) → Answer
rgbConstYes1 rgb = Yes
{-# COMPILE AGDA2SCALA rgbConstYes1 #-}

-- function with multiple named arguments

constRgbPair : (rgbPairArg : RgbPair) -> (rgbArg : Rgb) -> RgbPair
constRgbPair rgbPairArg rgbArg = rgbPairArg
{-# COMPILE AGDA2SCALA constRgbPair #-}

-- literals String
hello : String
hello = "Hello, world!"
{-# COMPILE AGDA2SCALA hello #-}

-- literals String with escape characters
withEscapes : String
withEscapes = "line1\nline2\t\"quote\"\\backslash"
{-# COMPILE AGDA2SCALA withEscapes #-}

-- literals Nat
two : Nat
two = 2
-- TODO {-# COMPILE AGDA2SCALA two #-}
-- {-# COMPILE AGDA2SCALA_DEBUG two #-}

-- polymorphic functions

id : {A : Set} -> A -> A
id x = x
{-# COMPILE AGDA2SCALA id #-}

data Maybe (A : Set) : Set where
  Just : (x : A) -> Maybe A
  None :            Maybe A
{-# COMPILE AGDA2SCALA Maybe #-}

data List (X : Set) : Set where
  []   : List X
  _::_ : X -> List X -> List X
{-# COMPILE AGDA2SCALA List #-}

--emptyNatList : List ℕ
--emptyNatList = []
-- {-# COMPILE AGDA2SCALA emptyNatList #-}
-- TODO handle variance

