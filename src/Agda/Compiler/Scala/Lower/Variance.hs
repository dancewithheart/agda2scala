module Agda.Compiler.Scala.Lower.Variance
  ( inferDataTyParams
  , inferParamVariance
  , Occurrence
  ) where

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaCtor(..)
  , ScalaName
  , ScalaTyParam(..)
  , ScalaType(..)
  , ScalaVariance(..)
  )

data Polarity
  = Positive
  | Negative
  deriving (Eq, Show)

data Occurrence
  = Absent
  | PositiveOnly
  | NegativeOnly
  | Mixed
  deriving (Eq, Show, Enum, Bounded)

instance Semigroup Occurrence where
  Absent <> other = other
  other <> Absent = other
  PositiveOnly <> PositiveOnly = PositiveOnly
  NegativeOnly <> NegativeOnly = NegativeOnly
  _ <> _ = Mixed

instance Monoid Occurrence where
  mempty = Absent

inferDataTyParams
  :: ScalaName
  -> [ScalaName]
  -> [ScalaCtor]
  -> [ScalaTyParam]
inferDataTyParams dataName params ctors =
  [ ScalaTyParam param (inferParamVariance dataName param ctors)
  | param <- params
  ]

inferParamVariance
  :: ScalaName
  -> ScalaName
  -> [ScalaCtor]
  -> ScalaVariance
inferParamVariance dataName param ctors =
  case evidence of
    Absent       -> Covariant
    PositiveOnly -> Covariant
    NegativeOnly -> Invariant
    Mixed        -> Invariant
  where
    evidence = foldMap occurrencesInCtor ctors
    occurrencesInCtor ctor = foldMap (occurrences dataName param Positive) (scArgs ctor)

occurrences
  :: ScalaName
  -> ScalaName
  -> Polarity
  -> ScalaType
  -> Occurrence
occurrences dataName param polarity scalaType =
  case scalaType of
    STyName _ -> Absent
    STyVar name
      | name == param -> occurrenceAt polarity
      | otherwise     -> Absent
    STyFun input output ->
      occurrences dataName param (flipPolarity polarity) input
      <> occurrences dataName param polarity output
    STyApp typeName args
      | typeName == dataName ->
          foldMap (occurrences dataName param polarity) args
      | any (mentions param) args -> Mixed
      | otherwise -> Absent

mentions :: ScalaName -> ScalaType -> Bool
mentions param scalaType =
  case scalaType of
    STyName _ -> False
    STyVar name -> name == param
    STyFun input output -> mentions param input || mentions param output
    STyApp _ args -> any (mentions param) args

occurrenceAt :: Polarity -> Occurrence
occurrenceAt polarity =
  case polarity of
    Positive -> PositiveOnly
    Negative -> NegativeOnly

flipPolarity :: Polarity -> Polarity
flipPolarity polarity =
  case polarity of
    Positive -> Negative
    Negative -> Positive
