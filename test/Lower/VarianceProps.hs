{-# LANGUAGE OverloadedStrings #-}

module Lower.VarianceProps (varianceProps) where

import Data.Foldable (traverse_)
import Hedgehog
  ( Group(..)
  , Property
  , property
  , (===)
  )
import Agda.Compiler.Scala.Lower.Variance (Occurrence)

varianceProps :: Group
varianceProps =
  Group "Terms / Env / application"
    [ ("occurrence evidence forms a monoid", prop_occurrence_isMonoid)
    ]

{-# ANN prop_occurrence_isMonoid ("HLint: ignore Monoid law, left identity" :: String) #-}
{-# ANN prop_occurrence_isMonoid ("HLint: ignore Monoid law, right identity" :: String) #-}
prop_occurrence_isMonoid :: Property
prop_occurrence_isMonoid = property $ do
  traverse_
    (\a -> do
      mempty <> a === a
      a <> mempty === a
    )
    allOccurrences
  traverse_
    (\(a, b, c) -> (a <> b) <> c === a <> (b <> c) )
    [ (a, b, c) | a <- allOccurrences, b <- allOccurrences , c <- allOccurrences ]

allOccurrences :: [Occurrence]
allOccurrences = [minBound .. maxBound]
