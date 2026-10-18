{-# LANGUAGE OverloadedStrings #-}

module Lower.VarianceTest (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaType (..)
  , ScalaTypeBinder (..)
  , ScalaTyParam (..)
  , ScalaVariance(..)
  , ScalaCtor (..)
  )
import Agda.Compiler.Scala.Lower.Variance ( inferDataTyParams )

tests :: TestTree
tests =
    testGroup
        "Lower.Variance"
        [ testCase "RBT is covariant" test_rbtParameterIsCovariant
        , testCase "Function input prevents covariance" test_negativeOccurrenceIsInvariant
        , testCase "Positive and negative is invariant" test_mixedOccurrenceIsInvariant
        , testCase "Unknown wrappers remain conservative" test_unknownTypeConstructorIsInvariant
        , testCase
            "higher-kinded parameters are invariant"
            test_higherKindedParameterIsInvariant
        ]

test_rbtParameterIsCovariant :: Assertion
test_rbtParameterIsCovariant =
  assertEqual
    "RBT value parameter occurs only covariantly"
    [ScalaTyParam (ScalaTypeBinder "V" 0) Covariant]
    (inferDataTyParams "RedBlackTree" [ScalaTypeBinder "V" 0] ctors)
  where
    v = STyVar "V"
    treeV = STyApp "RedBlackTree" [v]

    ctors =
      [ ScalaCtor "EmptyRBT" []
      , ScalaCtor
          "RBT"
          [ STyName "Color"
          , treeV
          , STyName "Long"
          , v
          , treeV
          ]
      ]

test_negativeOccurrenceIsInvariant :: Assertion
test_negativeOccurrenceIsInvariant =
  assertEqual
    "function input is a negative occurrence"
    [ScalaTyParam (ScalaTypeBinder "A" 0) Invariant]
    ( inferDataTyParams
        "Consumer"
        [ScalaTypeBinder "A" 0]
        [ ScalaCtor
            "Consumer"
            [STyFun (STyVar "A") (STyName "Long")]
        ]
    )

test_mixedOccurrenceIsInvariant :: Assertion
test_mixedOccurrenceIsInvariant =
  assertEqual
    "mixed occurrences require invariance"
    [ScalaTyParam (ScalaTypeBinder "A" 0) Invariant]
    ( inferDataTyParams
        "Endomorphism"
        [ScalaTypeBinder "A" 0]
        [ScalaCtor "Endomorphism" [STyFun (STyVar "A") (STyVar "A")]]
    )

test_unknownTypeConstructorIsInvariant :: Assertion
test_unknownTypeConstructorIsInvariant =
  assertEqual
    "unknown constructor variance is treated conservatively"
    [ScalaTyParam (ScalaTypeBinder "A" 0) Invariant]
    ( inferDataTyParams
        "Wrapped"
        [ScalaTypeBinder "A" 0]
        [ScalaCtor "Wrapped" [STyApp "UnknownContainer" [STyVar "A"]]]
    )

test_higherKindedParameterIsInvariant :: Assertion
test_higherKindedParameterIsInvariant =
  assertEqual
    "higher-kinded parameters remain invariant"
    [ ScalaTyParam (ScalaTypeBinder "F" 1) Invariant
    , ScalaTyParam (ScalaTypeBinder "A" 0) Invariant
    ]
    (inferDataTyParams "HKBox" params ctors)
  where
    params = [ ScalaTypeBinder "F" 1, ScalaTypeBinder "A" 0 ]
    ctors = [ ScalaCtor "Wrap" [STyApp "F" [STyVar "A"]] ]