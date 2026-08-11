module examples.Monoids where

open import Agda.Builtin.Equality using (_≡_)
open import examples.higher_order_fun using (List; []; _::_)
open import Data.Nat using (ℕ; _+_; zero; suc)

record Monoid (A : Set) : Set where
  field
    -- operations
    empty   : A
    _⊕_ : A -> A -> A
{-# COMPILE AGDA2SCALA Monoid #-}

record MonoidLaws (A : Set) : Set where
  field
    monoid : Monoid A
  open Monoid monoid  
  field
    -- laws
    right-unit : forall (x : A) -> x ⊕ empty ≡ x
    left-unit  : forall (x : A) -> empty ⊕ x ≡ x
    assoc      : forall (x y z : A) -> (x ⊕ y) ⊕ z ≡ x ⊕ (y ⊕ z)
{-# COMPILE AGDA2SCALA MonoidLaws #-}

NatPlusMonoid : Monoid ℕ
NatPlusMonoid = record
  { empty = zero
  ; _⊕_ = _+_
  }
{-# COMPILE AGDA2SCALA NatPlusMonoid #-}

{-
NatPlusMonoidLaws : MonoidLaws ℕ
NatPlusMonoidLaws = record
  { monoid =  NatPlusMonoid
  ; right-unit = {!!}
  ; left-unit = {!!}
  ; assoc = {!!}
  }
-}

fold : {A : Set} -> Monoid A -> List A -> A
fold M []        = Monoid.empty M
fold M (x :: xs) = x ⊕ (fold M xs)
  where
   open Monoid M
{-# COMPILE AGDA2SCALA fold #-}

