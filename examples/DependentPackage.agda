module examples.DependentPackage where

record Package : Set₁ where
  field
    Carrier : Set
    item    : Carrier

{-# COMPILE AGDA2SCALA Package = path #-}
