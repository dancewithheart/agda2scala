module examples.hk_typs where

open import Agda.Builtin.Nat

record NatTransformer (F : Set → Set) : Set₁ where
  field
    run : F Nat → F Nat
{-# COMPILE AGDA2SCALA NatTransformer #-}

polyId : {F : Set → Set} {A : Set} → F A → F A
polyId x = x
{-# COMPILE AGDA2SCALA polyId #-}
{-# COMPILE AGDA2SCALA_DEBUG polyId #-}
