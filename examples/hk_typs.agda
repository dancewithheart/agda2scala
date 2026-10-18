module examples.hk_typs where

open import Agda.Builtin.Nat

record NatTransformer (F : Set → Set) : Set₁ where
  field
    run : F Nat → F Nat
{-# COMPILE AGDA2SCALA NatTransformer #-}

polyId : {F : Set → Set} {A : Set} → F A → F A
polyId x = x
{-# COMPILE AGDA2SCALA polyId #-}

data HKBox (F : Set → Set) (A : Set) : Set₁ where
  Wrap : F A → HKBox F A
{-# COMPILE AGDA2SCALA HKBox #-}
{-# COMPILE AGDA2SCALA_DEBUG HKBox #-}
