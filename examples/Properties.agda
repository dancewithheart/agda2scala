module examples.Properties where

open import Data.Nat using (ℕ; _+_; zero)
open import Agda.Builtin.Equality using (_≡_)
open import Data.Nat.Properties using (+-identityʳ)

addZero : ℕ → ℕ
addZero x = x + zero
{-# COMPILE AGDA2SCALA addZero #-}

addZeroLaw : (x : ℕ) → addZero x ≡ x
addZeroLaw x = +-identityʳ x
{-# COMPILE AGDA2SCALA addZeroLaw #-}
