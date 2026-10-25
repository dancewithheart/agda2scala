{-# OPTIONS --erasure #-}
module examples.ScalaErasedIf where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Equality using (_≡_; refl)

if_then_else_ : ∀ {A : Set} (b : Bool) → (@0 (b ≡ true) → A) → (@0 (b ≡ false) → A) → A
if true then yes else no = yes refl
if false then yes else no = no refl

ifChoice : Bool → Bool
ifChoice b = if b then (λ _ → true) else (λ _ → false)
{-# COMPILE AGDA2SCALA ifChoice #-}

caseChoice : Bool → Bool
caseChoice false = false
caseChoice true = true
{-# COMPILE AGDA2SCALA caseChoice #-}
