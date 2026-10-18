module examples.hk_typs where

open import Agda.Builtin.Nat

record NatTransformer (F : Set → Set) : Set₁ where
  field
    run : F Nat → F Nat
{-# COMPILE AGDA2SCALA NatTransformer #-}
{-# COMPILE AGDA2SCALA_DEBUG NatTransformer #-}
