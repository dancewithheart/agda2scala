module examples.ScalaUnsupported where

open import Agda.Builtin.Bool

postulate external : Bool
{-# COMPILE AGDA2SCALA external #-}
