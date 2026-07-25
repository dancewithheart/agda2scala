{-# LANGUAGE OverloadedStrings #-}

module Lower.VarianceTest (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaType (..)
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
        ]

test_rbtParameterIsCovariant :: Assertion
test_rbtParameterIsCovariant =
  assertEqual
    "RBT value parameter occurs only covariantly"
    [ScalaTyParam "V" Covariant]
    (inferDataTyParams "RedBlackTree" ["V"] ctors)
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
    [ScalaTyParam "A" Invariant]
    ( inferDataTyParams
        "Consumer"
        ["A"]
        [ ScalaCtor
            "Consumer"
            [STyFun (STyVar "A") (STyName "Long")]
        ]
    )

test_mixedOccurrenceIsInvariant :: Assertion
test_mixedOccurrenceIsInvariant =
  assertEqual
    "mixed occurrences require invariance"
    [ScalaTyParam "A" Invariant]
    ( inferDataTyParams
        "Endomorphism"
        ["A"]
        [ScalaCtor "Endomorphism" [STyFun (STyVar "A") (STyVar "A")]]
    )

test_unknownTypeConstructorIsInvariant :: Assertion
test_unknownTypeConstructorIsInvariant =
  assertEqual
    "unknown constructor variance is treated conservatively"
    [ScalaTyParam "A" Invariant]
    ( inferDataTyParams
        "Wrapped"
        ["A"]
        [ScalaCtor "Wrapped" [STyApp "UnknownContainer" [STyVar "A"]]]
    )
