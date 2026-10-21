module examples.Coyoneda where

open import Data.Nat using (ℕ; _+_)
open import Agda.Builtin.Maybe using (Maybe; just)

record Coyoneda (F : Set → Set) (A : Set) : Set₁ where
  field
    X  : Set
    fx : F X
    k  : X → A

{-# COMPILE AGDA2SCALA Coyoneda = path #-}

addOne : ℕ → ℕ
addOne x = x + 1
{-# COMPILE AGDA2SCALA addOne #-}

example : Coyoneda Maybe ℕ
example = record
  { X  = ℕ
  ; fx = just 41
  ; k  = addOne
  }
{-# COMPILE AGDA2SCALA example #-}
