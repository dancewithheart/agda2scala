module examples.DependentPackage where

open import Data.Nat using (ℕ)

record Package : Set₁ where
  field
    Carrier : Set
    item    : Carrier

{-# COMPILE AGDA2SCALA Package = path #-}

example : Package
example = record
  { Carrier = ℕ
  ; item    = 42
  }
{-# COMPILE AGDA2SCALA example #-}

