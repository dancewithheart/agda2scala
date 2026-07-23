module examples.rbt where

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Nat using (ℕ; _<ᵇ_)

data Color : Set where
  Red Black : Color
{-# COMPILE AGDA2SCALA Color #-}

data RedBlackTree (V : Set) : Set where
  EmptyRBT : RedBlackTree V
  RBT      : Color -> RedBlackTree V -> ℕ -> V -> RedBlackTree V -> RedBlackTree V
{-# COMPILE AGDA2SCALA RedBlackTree #-}

lookup : {V : Set} -> V -> ℕ -> RedBlackTree V -> V
lookup defaultVal key EmptyRBT =
  defaultVal
lookup defaultVal key (RBT c left currKey x right) =
  if key <ᵇ currKey then
    lookup defaultVal key left
  else
    if currKey <ᵇ key then
      lookup defaultVal key right
    else
      x
{-# COMPILE AGDA2SCALA lookup #-}

checkR : {V : Set} -> ℕ -> RedBlackTree V -> V -> RedBlackTree V -> RedBlackTree V
checkR k tl vk (RBT Red (RBT Red b y vy tr2) z vz tr3) =
  RBT Red (RBT Black tl k vk b) y vy (RBT Black tr2 z vz tr3)
checkR k tl vk (RBT Red EmptyRBT y vy (RBT Red tl2 z vz tr2)) =
  RBT Red (RBT Black tl k vk EmptyRBT) y vy (RBT Black tl2 z vz tr2)
checkR k tl vk (RBT Red (RBT Black b1 y1 vy1 b2) y vy (RBT Red tl2 z vz tr2)) =
  RBT Red (RBT Black tl k vk (RBT Black b1 y1 vy1 b2)) y vy (RBT Black tl2 z vz tr2)
checkR k tl vk tr =
  RBT Black tl k vk tr
{-# COMPILE AGDA2SCALA checkR #-}

checkL : {V : Set} -> ℕ -> RedBlackTree V -> V -> RedBlackTree V -> RedBlackTree V
checkL k (RBT Red (RBT Red a x vx b) y vy c) vk tr =
  RBT Red (RBT Black a x vx b) y vy (RBT Black c k vk tr)
checkL k (RBT Red EmptyRBT x vx (RBT Red b y vy c)) vk tr =
  RBT Red (RBT Black EmptyRBT x vx b) y vy (RBT Black c k vk tr)
checkL k (RBT Red (RBT Black a1 x1 vx1 a2) x vx (RBT Red b y vy c)) vk tr =
  RBT Red (RBT Black (RBT Black a1 x1 vx1 a2) x vx b) y vy (RBT Black c k vk tr)
checkL k tl vk tr = checkR k tl vk tr
{-# COMPILE AGDA2SCALA checkL #-}

balance : {V : Set} -> Color -> RedBlackTree V -> ℕ -> V -> RedBlackTree V -> RedBlackTree V
balance Red tl k vk tr   = RBT Red tl k vk tr
balance Black tl k vk tr = checkL k tl vk tr
{-# COMPILE AGDA2SCALA balance #-}

ins : {V : Set} -> ℕ -> V -> RedBlackTree V -> RedBlackTree V
ins key val EmptyRBT = RBT Red EmptyRBT key val EmptyRBT
ins key val (RBT c tl k2 v2 tr) =
  if key <ᵇ k2 then
    balance c (ins key val tl) k2 v2 tr
  else
    if k2 <ᵇ key then
      balance c tl k2 v2 (ins key val tr)
    else
      RBT c tl key val tr
{-# COMPILE AGDA2SCALA ins #-}

makeBlack : {V : Set} -> RedBlackTree V -> RedBlackTree V
makeBlack EmptyRBT = EmptyRBT
makeBlack (RBT c tl k val tr) = RBT Black tl k val tr
{-# COMPILE AGDA2SCALA makeBlack #-}

insert : {V : Set} -> ℕ -> V -> RedBlackTree V -> RedBlackTree V
insert k val t = makeBlack (ins k val t)
{-# COMPILE AGDA2SCALA insert #-}
