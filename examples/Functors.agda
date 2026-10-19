module examples.Functors where

open import Agda.Builtin.Maybe using (Maybe; nothing; just)
open import Agda.Builtin.List using (List; []; _∷_)

record Functor (F : Set → Set) : Set₁ where
  field
    map : {A B : Set} → (A → B) → F A → F B
{-# COMPILE AGDA2SCALA Functor #-}

mapMaybe : {A B : Set} → (A → B) → Maybe A → Maybe B
mapMaybe f nothing  = nothing
mapMaybe f (just x) = just (f x)
{-# COMPILE AGDA2SCALA mapMaybe #-}

mapList : {A B : Set} → (A → B) → List A → List B
mapList f []       = []
mapList f (x ∷ xs) = f x ∷ mapList f xs
{-# COMPILE AGDA2SCALA mapList #-}

optionFunctor : Functor Maybe
optionFunctor = record { map = mapMaybe }
{-# COMPILE AGDA2SCALA optionFunctor #-}

listFunctor : Functor List
listFunctor = record { map = mapList }
{-# COMPILE AGDA2SCALA listFunctor #-}
