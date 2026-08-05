module examples.higher_order_fun where

open import Data.Bool using (Bool; false; true)
open import Data.Nat using (ℕ)

-- polymorphic functions

id : {A : Set} -> A -> A
id x = x
{-# COMPILE AGDA2SCALA id #-}

-- polymorphic ADTs

data Maybe (A : Set) : Set where
  Just : (x : A) -> Maybe A
  None :            Maybe A
{-# COMPILE AGDA2SCALA Maybe #-}

data List (X : Set) : Set where
  []   : List X
  _::_ : X -> List X -> List X
{-# COMPILE AGDA2SCALA List #-}

emptyNatList : List ℕ
emptyNatList = []
{-# COMPILE AGDA2SCALA emptyNatList #-}

map : {A B : Set} ->  (A -> B) -> List A -> List B
map f []        = []
map f (x :: xs) = f x :: map f xs
{-# COMPILE AGDA2SCALA map #-}

foldr : {A B : Set} ->  (A -> B -> B) -> B -> List A -> B
foldr f init []        = init
foldr f init (x :: xs) = f (foldr f init xs)
{-# COMPILE AGDA2SCALA foldr #-}

filter : {A : Set} -> (A -> Bool) -> List A -> List A
filter f []        = []
filter f (x :: xs) with f x
... | false = filter f xs
... | true  = x :: (filter f xs)
{-# COMPILE AGDA2SCALA filter #-}

maybe : {A B : Set} -> B -> (A -> B) -> Maybe A -> B
maybe b f (Just a) = f a
maybe b f None     = b
{-# COMPILE AGDA2SCALA maybe #-}
