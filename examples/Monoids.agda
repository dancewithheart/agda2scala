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


-- Next PR: compile record construction.

NatPlusMonoid : Monoid ℕ
NatPlusMonoid = record
  { empty   = zero
  ; combine = _+_
  }


-- Later: compile selected equality propositions into property tests.

record MonoidLaws (A : Set) : Set where
  field
    monoid : Monoid A

  open Monoid monoid

  field
    right-unit : (x : A) → combine x empty ≡ x
    left-unit  : (x : A) → combine empty x ≡ x

    assoc : (x y z : A) →
      combine (combine x y) z ≡
      combine x (combine y z)


NatPlusMonoidLaws : MonoidLaws ℕ
NatPlusMonoidLaws = record
  { monoid     = NatPlusMonoid
  ; right-unit = +-identityʳ
  ; left-unit  = +-identityˡ
  ; assoc      = +-assoc
  }
